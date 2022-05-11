{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Api.DraftSpec where

import App.Result
import App.ResultJSON
import App.Types
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.Coerce
import Data.Data (Typeable)
import Data.Either
import Data.IntMap qualified as IM
import Data.Kind (Type)
import Data.List
import Data.Maybe
import Data.String
import Entity.Article
import Entity.Author
import Entity.Draft
import Entity.Internal
import Entity.User
import Extended.Text (Text)
import Extended.Text qualified as T
import HKD.HKD
import Helpers.App
import Helpers.Database
import Helpers.Draft
import Helpers.DraftDB
import Helpers.Entity
import Helpers.GenericProps
import Helpers.Internal
import Helpers.Monad
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  postSpec
  getSpec
  putSpec
  deleteSpec
  publishSpec

postSpec :: Spec
postSpec = describe "POST" $ do
  it "When all is ok it posts draft into the database" $
    property $ propPostsDraft

  it "Throws error when it fails to parse request body" $
    property $ propPostsParsingFail @Draft "drafts"

  it "Throws error when no user token is provided" $
    property $ propPostsDraftNoToken

  it "Throws error when wrong user token is provided" $
    property $ propPostsDraftWrongToken

propPostsDraft :: TDB Draft -> TDB User -> Draft Create -> Property
propPostsDraft dbD dbU draft =
  property $
    conditions ==> do
      (Right (ResText t), st) <-
        runTest
          ( withBody (toFrontCreate draft)
              . withPostPath "drafts"
              . withToken userToken
          )
          ( withDatabase @Draft dbD
              . withDatabase @User dbU
          )
      let Right (ID eID) = T.read @(ID (Draft Display)) t
      title . unDraft
        <$> IM.lookup eID (extractTestDatabaseFromTestState @(Draft Create) st)
          `shouldBe` Just (title $ unDraft draft)
  where
    conditions = not (alreadyExists draft dbD) && not (IM.null dbU) && uniqueUser
    uniqueUser = length (IM.filter ((== userToken) . token) dbU) == 1
    userToken = token . snd . head $ IM.toList dbU

propPostsDraftNoToken :: Draft (Front Create) -> Property
propPostsDraftNoToken draft = property $ do
  Left err <- evalTest (withBody draft . withPostPath "drafts") id
  err `shouldSatisfy` isUnathorizedError

propPostsDraftWrongToken :: Draft (Front Create) -> Token -> TDB User -> Property
propPostsDraftWrongToken draft t dbU =
  property $
    condition ==> do
      Left err <- evalTest (withBody draft . withToken t . withPostPath "drafts") id
      err `shouldSatisfy` isEntityNotFoundError
  where
    condition = null $ IM.filter ((== t) . token) dbU

getSpec :: Spec
getSpec = describe "GET" $ do
  context "many" $ do
    it "When all is ok it gets list of drafts" $
      property propGetDrafts

    it "This list is paginated" $
      property propGetDraftsIsPaginated

    it "Allows to get various pages of this list" $
      property propGetDraftsWithlimitOffset

    it "Throws error when no token is provided" $
      property propGetDraftsNoToken

  context "single" $ do
    it "When all is ok it allows to get an draft by its own ID" $
      property propGetDraft

    it "Throws error when draft with this ID doesn't exists" $
      property propGetDraftDoesntExists

    it "Throws error when no token is provided" $
      property propGetDraftNoToken

    it "Throws error when wrong token is provided" $
      property propGetDraftWrongToken

propGetDrafts :: TDB Draft -> Entity User Display -> Property
propGetDrafts dbD e =
  property $
    condition ==> do
      Right (ResJSON j) <-
        evalTest
          ( withGetPath "drafts"
              . withToken (token u)
          )
          ( withDatabase @Draft dbD
              . withDatabase @User dbU
          )
      let Right ref' = runTestMonadNoMods $ toJSONResult ref
      j `shouldBe` encode ref'
  where
    condition = length (IM.toList dbD) < testPaginationConstant
    ref =
      map (\(a, b) -> Entity (ID a) (fromDisplay @(Draft (Front Display)) b)) $
        IM.toList $
          IM.filter ((== ID userID) . entityID . user . entity . author . unDraft) dbD
    Entity (ID userID) u = e
    dbU = IM.singleton userID u

propGetDraftsIsPaginated :: BigTDB Draft -> Entity User Display -> Property
propGetDraftsIsPaginated (unBigTDB -> dbD) e =
  property $
    condition ==> do
      Right (ResJSON j) <-
        evalTest
          ( withGetPath "drafts"
              . withToken (token u)
          )
          ( withDatabase @Draft dbD
              . withDatabase @User dbU
          )
      let Just xs = decode @[Value] j
      length xs <= testPaginationConstant `shouldBe` True
  where
    condition = length (IM.toList dbD) > testPaginationConstant
    ref =
      map (\(a, b) -> Entity (ID a) (fromDisplay @(Draft (Front Display)) b)) $
        IM.toList $
          IM.filter ((== ID userID) . entityID . user . entity . author . unDraft) dbD
    Entity (ID userID) u = e
    dbU = IM.singleton userID u

propGetDraftsWithlimitOffset :: TDB Draft -> Entity User Display -> Int -> Int -> Property
propGetDraftsWithlimitOffset dbD e limit offset =
  property $
    condition ==> do
      Right (ResJSON j) <-
        evalTest
          ( withGetPath "drafts"
              . withToken (token u)
              . withLimit limit
              . withOffset offset
          )
          ( withDatabase @Draft dbD
              . withDatabase @User dbU
          )
      let Right ref' = runTestMonadNoMods $ toJSONResult ref
      j `shouldBe` encode ref'
  where
    condition = length (IM.toList dbD) < testPaginationConstant
    ref =
      paginate $
        map (\(a, b) -> Entity (ID a) (fromDisplay @(Draft (Front Display)) b)) $
          IM.toList $
            IM.filter ((== ID userID) . entityID . user . entity . author . unDraft) dbD
    Entity (ID userID) u = e
    dbU = IM.singleton userID u
    paginate = take (min limit testPaginationConstant) . drop offset

propGetDraftsNoToken :: Property
propGetDraftsNoToken = property $ do
  Left err <-
    evalTest
      (withGetPath "drafts")
      id
  err `shouldSatisfy` isUnathorizedError

propGetDraft :: TDB Draft -> ID (Draft Display) -> User Display -> Property
propGetDraft dbD (ID draftID) u =
  property $
    conditions ==> do
      Right (ResJSON j) <-
        evalTest
          ( withGetPath ("drafts/" <> T.show draftID)
              . withToken t
          )
          ( withDatabase @Draft dbD
              . withDatabase @User dbU
          )
      j ^? key "title" . _String
        `shouldBe` title . unDraft <$> IM.lookup draftID dbD
  where
    conditions = draftID `elem` IM.keys dbD
    Just t = token . entity . user . entity . author . unDraft <$> IM.lookup draftID dbD
    Just (ID userID) = entityID . user . entity . author . unDraft <$> IM.lookup draftID dbD
    dbU = IM.singleton userID $ (#token .~ t) u

propGetDraftDoesntExists :: TDB Draft -> ID (Draft Display) -> Entity User Display -> Property
propGetDraftDoesntExists dbD (ID draftID) e =
  property $
    conditions ==> do
      Left err <-
        evalTest
          ( withGetPath ("drafts/" <> T.show draftID)
              . withToken (token u)
          )
          ( withDatabase @Draft dbD
              . withDatabase @User dbU
          )
      err `shouldSatisfy` isEntityNotFoundError
  where
    conditions = draftID `notElem` IM.keys dbD
    dbU = IM.singleton userID u
    Entity (ID userID) u = e

propGetDraftNoToken :: ID (Draft Display) -> Property
propGetDraftNoToken (ID draftID) = property $ do
  Left res <-
    evalTest
      (withGetPath ("drafts/" <> T.show draftID))
      id
  res `shouldSatisfy` isUnathorizedError

propGetDraftWrongToken :: TDB Draft -> ID (Draft Display) -> User Display -> Token -> Property
propGetDraftWrongToken dbD (ID draftID) u wrongT =
  property $
    conditions ==> do
      Left err <-
        evalTest
          ( withGetPath ("drafts/" <> T.show draftID)
              . withToken wrongT
          )
          ( withDatabase @Draft dbD
              . withDatabase @User dbU
          )
      err `shouldSatisfy` isEntityNotFoundError
  where
    conditions = draftID `elem` IM.keys dbD && wrongT /= t
    Just t = token . entity . user . entity . author . unDraft <$> IM.lookup draftID dbD
    Just (ID userID) = entityID . user . entity . author . unDraft <$> IM.lookup draftID dbD
    dbU = IM.singleton userID $ (#token .~ t) u

putSpec :: Spec
putSpec = describe "PUT" $ do
  it "Actually changes draft" $
    property propPutDraft

  it "Throws error when draft with this ID doesn't exists" $
    property propPutDraftDoesntExists

  it "Throws error when it fails to parse request body" $
    property propPutDraftParsingFail

  it "Throws error when no token is provided" $
    property propPutDraftNoToken

  it "Throws error when wrong token is provided" $
    property propPutDraftWrongToken

propPutDraft ::
  TDB Draft ->
  Draft (Front Update) ->
  ID (Draft Display) ->
  User Display ->
  Property
propPutDraft dbD draftU (ID draftID) u =
  property $
    conditions ==> do
      st <-
        execTest
          ( withBody draftU
              . withPutPath ("drafts/" <> T.show draftID)
              . withToken t
          )
          ( withDatabase @Draft dbD
              . withDatabase @User dbU
          )
      IM.lookup draftID (_tsDraftDB st) `shouldBe` Just (draftU >/ oldDraft)
  where
    conditions = draftID `elem` IM.keys dbD
    Just oldDraft = IM.lookup draftID dbD
    dbU = IM.singleton userID $ (#token .~ t) u
    Just t = token . entity . user . entity . author . unDraft <$> IM.lookup draftID dbD
    Just (ID userID) = entityID . user . entity . author . unDraft <$> IM.lookup draftID dbD

propPutDraftDoesntExists ::
  TDB Draft ->
  Draft (Front Update) ->
  ID (Draft Display) ->
  Entity User Display ->
  Property
propPutDraftDoesntExists dbD draftU (ID draftID) e =
  property $
    conditions ==> do
      Left err <-
        evalTest
          ( withBody draftU
              . withPutPath ("drafts/" <> T.show draftID)
              . withToken (token u)
          )
          ( withDatabase @Draft dbD
              . withDatabase @User dbU
          )
      err `shouldSatisfy` isEntityNotFoundError
  where
    conditions = draftID `notElem` IM.keys dbD
    dbU = IM.singleton userID u
    Entity (ID userID) u = e

propPutDraftParsingFail ::
  TDB Draft ->
  Value ->
  ID (Draft Display) ->
  User Display ->
  Property
propPutDraftParsingFail dbD (fromString . show -> obj) (ID draftID) u =
  property $
    conditions ==> do
      Left err <-
        evalTest
          ( withBLBody obj
              . withPutPath ("drafts/" <> T.show draftID)
              . withToken (token u)
          )
          ( withDatabase @Draft dbD
              . withDatabase @User dbU
          )
      err `shouldSatisfy` isParsingError
  where
    conditions = draftID `elem` IM.keys dbD && isNothing (decode @(Draft (Front Update)) obj)
    dbU = IM.singleton userID u
    Just t = token . entity . user . entity . author . unDraft <$> IM.lookup draftID dbD
    Just (ID userID) = entityID . user . entity . author . unDraft <$> IM.lookup draftID dbD

propPutDraftNoToken :: ID (Draft Display) -> Property
propPutDraftNoToken (ID draftID) = property $ do
  Left res <-
    evalTest
      (withPutPath ("drafts/" <> T.show draftID))
      id
  res `shouldSatisfy` isUnathorizedError

propPutDraftWrongToken ::
  TDB Draft ->
  Draft (Front Update) ->
  ID (Draft Display) ->
  User Display ->
  Token ->
  Property
propPutDraftWrongToken dbD draftU (ID draftID) u wrongToken =
  property $
    conditions ==> do
      Left err <-
        evalTest
          ( withBody draftU
              . withPutPath ("drafts/" <> T.show draftID)
              . withToken wrongToken
          )
          ( withDatabase @Draft dbD
              . withDatabase @User dbU
          )
      err `shouldSatisfy` isEntityNotFoundError
  where
    conditions = draftID `elem` IM.keys dbD && wrongToken /= t
    Just oldDraft = IM.lookup draftID dbD
    dbU = IM.singleton userID $ (#token .~ t) u
    Just t = token . entity . user . entity . author . unDraft <$> IM.lookup draftID dbD
    Just (ID userID) = entityID . user . entity . author . unDraft <$> IM.lookup draftID dbD

deleteSpec :: Spec
deleteSpec = describe "DELETE" $ do
  it "Actually deletes draft from database" $
    property $ propDeleteDraft

  it "Throws error when draft with this ID doesn't exists" $
    property $ propDeleteDraftDoesntExists

  it "Throws error when no token is provided" $
    property $ propDeleteDraftNoToken

  it "Throws error when wrong token is provided" $
    property $ propDeleteDraftWrongToken

propDeleteDraft :: TDB Draft -> ID (Draft Display) -> User Display -> Property
propDeleteDraft dbD (ID draftID) u =
  property $
    conditions ==> do
      st <-
        execTest
          (withDeletePath ("drafts/" <> T.show draftID) . withToken t)
          (withDatabase @Draft dbD . withDatabase @User dbU)
      extractTestDatabaseFromTestState @(Draft Display) st `shouldBe` IM.delete draftID dbD
  where
    Just (ID userID) = entityID . user . entity . author . unDraft <$> IM.lookup draftID dbD
    conditions = draftID `elem` IM.keys dbD
    dbU = IM.singleton userID $ (#token .~ t) u
    Just t = token . entity . user . entity . author . unDraft <$> IM.lookup draftID dbD

propDeleteDraftDoesntExists :: TDB Draft -> ID (Draft Display) -> Entity User Display -> Property
propDeleteDraftDoesntExists dbD (ID draftID) e =
  property $
    conditions ==> do
      Left res <-
        evalTest
          ( withDeletePath ("drafts/" <> T.show draftID)
              . withToken (token u)
          )
          (withDatabase @Draft dbD . withDatabase @User dbU)
      res `shouldSatisfy` isEntityNotFoundError
  where
    conditions = draftID `notElem` IM.keys dbD
    dbU = IM.singleton userID u
    Entity (ID userID) u = e

propDeleteDraftNoToken :: ID (Draft Display) -> Property
propDeleteDraftNoToken (ID draftID) = property $ do
  Left res <-
    evalTest
      (withDeletePath ("drafts/" <> T.show draftID))
      id
  res `shouldSatisfy` isUnathorizedError

propDeleteDraftWrongToken :: TDB Draft -> ID (Draft Display) -> User Display -> Token -> Property
propDeleteDraftWrongToken dbD (ID draftID) u wrongT =
  property $
    conditions ==> do
      Left res <-
        evalTest
          ( withDeletePath ("drafts/" <> T.show draftID)
              . withToken wrongT
          )
          ( withDatabase @Draft dbD
              . withDatabase @User dbU
          )
      res `shouldSatisfy` isEntityNotFoundError
  where
    Just (ID userID) = entityID . user . entity . author . unDraft <$> IM.lookup draftID dbD
    conditions = draftID `elem` IM.keys dbD && wrongT /= t
    dbU = IM.singleton userID $ (#token .~ t) u
    Just t = token . entity . user . entity . author . unDraft <$> IM.lookup draftID dbD

publishSpec :: Spec
publishSpec = describe "PUBLISH" $ do
  it "Actually publishes a draft" $
    property propPublishDraft

  it "Throws error when draft with this ID doesn't exists" $
    property propPublishDraftDoesntExists

  it "Throws error when no token is provided" $
    property propPublishDraftNoToken

  it "Throws error when wrong token is provided" $
    property propPublishDraftWrongToken

propPublishDraft :: TDB Draft -> ID (Draft Display) -> User Display -> Property
propPublishDraft dbD (ID draftID) u =
  property $
    conditions ==> do
      st <-
        execTest
          ( withPublishPath ("drafts/" <> T.show draftID)
              . withToken t
          )
          ( withDatabase @Draft dbD
              . withDatabase @User dbU
          )
      draftID `elem` IM.keys (_tsDraftDB st) `shouldBe` False
      IM.elems (_tsArticleDB st) `shouldBe` [ref]
  where
    conditions = draftID `elem` IM.keys dbD
    Just (ID userID) = entityID . user . entity . author . unDraft <$> IM.lookup draftID dbD
    Just t = token . entity . user . entity . author . unDraft <$> IM.lookup draftID dbD
    dbU = IM.singleton userID $ (#token .~ t) u
    Just ref = unDraft <$> IM.lookup draftID dbD

propPublishDraftDoesntExists :: TDB Draft -> ID (Draft Display) -> Entity User Display -> Property
propPublishDraftDoesntExists dbD (ID draftID) e =
  property $
    conditions ==> do
      Left err <-
        evalTest
          ( withPublishPath ("drafts/" <> T.show draftID)
              . withToken (token u)
          )
          ( withDatabase @Draft dbD
              . withDatabase @User dbU
          )
      err `shouldSatisfy` isEntityNotFoundError
  where
    conditions = draftID `notElem` IM.keys dbD
    dbU = IM.singleton userID u
    Entity (ID userID) u = e

propPublishDraftNoToken :: ID (Draft Display) -> Property
propPublishDraftNoToken (ID draftID) = property $ do
  Left err <-
    evalTest
      (withPublishPath ("drafts/" <> T.show draftID))
      id
  err `shouldSatisfy` isUnathorizedError

propPublishDraftWrongToken :: TDB Draft -> ID (Draft Display) -> User Display -> Token -> Property
propPublishDraftWrongToken dbD (ID draftID) u wrongToken =
  property $
    conditions ==> do
      Left err <-
        evalTest
          ( withPublishPath ("drafts/" <> T.show draftID)
              . withToken wrongToken
          )
          ( withDatabase @Draft dbD
              . withDatabase @User dbU
          )
      err `shouldSatisfy` isEntityNotFoundError
  where
    conditions = draftID `elem` IM.keys dbD && wrongToken /= t
    Just (ID userID) = entityID . user . entity . author . unDraft <$> IM.lookup draftID dbD
    Just t = token . entity . user . entity . author . unDraft <$> IM.lookup draftID dbD
    dbU = IM.singleton userID $ (#token .~ t) u
    Just ref = unDraft <$> IM.lookup draftID dbD

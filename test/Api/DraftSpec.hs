{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module Api.DraftSpec where

import App.Result (AppResult (ResJSON, ResText))
import App.Types (ID, Token)
import Data.Aeson (eitherDecode, encode)
import Data.ByteString.Lazy qualified as BL
import Data.Either (isLeft)
import Database.Internal qualified as Database
import Entity.Article (Article (author))
import Entity.Author (Author (user))
import Entity.Draft (Draft (unDraft))
import Entity.Internal (Entity (..))
import Entity.User (User)
import Extended.Text (Text)
import Extended.Text qualified as T
import HKD.HKD (Create, Delete, Display, Front, Publish, Update)
import Mocks.Constant (testPaginationConstant)
import Mocks.Predicates
  ( isEntityNotFoundError,
    isParsingError,
    isUnathorizedError,
  )
import Mocks.Run (evalTest)
import Mocks.TestMonad
  ( TestEntity (withGetEntities),
    defaultPostResult,
  )
import Mocks.With
  ( withBLBody,
    withBody,
    withDeletePath,
    withGetPath,
    withLimit,
    withOffset,
    withPostPath,
    withPublishPath,
    withPutPath,
    withToken,
  )
import Test.Hspec
  ( Spec,
    context,
    describe,
    it,
    shouldBe,
    shouldSatisfy,
  )
import Test.QuickCheck (Property, Testable (property), (==>))

spec :: Spec
spec = do
  postSpec
  getSpec
  putSpec
  deleteSpec
  publishSpec

postSpec :: Spec
postSpec = describe "POST" $ do
  it "Actually posts draft into database when all is ok" $
    property propPostDraft
  it "Throws an appropriate error when no token is provided" $
    property propPostDraftNoToken
  it "Throws an appropriate error when wrong token is provided" $
    property propPostDraftWrongToken

propPostDraft :: Draft (Front Create) -> Token -> Entity User (Front Display) -> Property
propPostDraft draft t user = property $ do
  res <-
    evalTest
      ( withPostPath "drafts"
          . withBody draft
          . withToken t
      )
      (withGetEntities @(Entity User (Front Display)) [user])
  res `shouldBe` Right (ResText $ T.show defaultPostResult)

propPostDraftNoToken :: Draft (Front Create) -> Property
propPostDraftNoToken draft = property $ do
  Left err <-
    evalTest
      ( withPostPath "drafts"
          . withBody draft
      )
      id
  err `shouldSatisfy` isUnathorizedError

propPostDraftWrongToken :: Draft (Front Create) -> Token -> Property
propPostDraftWrongToken draft t = property $ do
  Left err <-
    evalTest
      ( withPostPath "drafts"
          . withBody draft
          . withToken t
      )
      id
  err `shouldSatisfy` isEntityNotFoundError

getSpec :: Spec
getSpec = describe "GET" $ do
  context "single" $ do
    it "Allows to get draft from database when all is ok" $
      property propGetDraft
    it "Throws an appropriate error when there is no draft with that ID" $
      property propGetDraftDoesntExists
    it "Throws an appropriate error when no token is provided" $
      property propGetDraftNoToken
    it "Throws an appropriate error when wrong token is provided" $
      property propGetDraftWrongToken
  context "many" $ do
    it "Allows to get paginated list of drafts when all is ok" $
      property propGetDrafts
    it "Supports arbitrary limit & offset" $
      property propGetDraftsArbitrary
    it "Throws an appropriate error when no token is provided" $
      property propGetDraftsNoToken

propGetDraft :: Entity Draft (Front Display) -> User (Front Display) -> Token -> Property
propGetDraft de@(Entity dID draft) u t = property $ do
  let uID = entityID . user . entity . author $ unDraft draft
  res <-
    evalTest
      ( withGetPath ("drafts/" <> T.show dID)
          . withToken t
      )
      ( withGetEntities @(Entity User (Front Display)) [Entity uID u]
          . withGetEntities @(Entity Draft (Front Display)) [de]
      )
  res `shouldBe` Right (ResJSON $ encode de)

propGetDraftDoesntExists :: Entity Draft (Front Display) -> User (Front Display) -> Token -> Property
propGetDraftDoesntExists de@(Entity dID draft) u t = property $ do
  let uID = entityID . user . entity . author $ unDraft draft
  Left err <-
    evalTest
      ( withGetPath ("drafts/" <> T.show dID)
          . withToken t
      )
      ( withGetEntities @(Entity User (Front Display)) [Entity uID u]
      )
  err `shouldSatisfy` isEntityNotFoundError

propGetDraftNoToken :: ID (Draft (Front Display)) -> Property
propGetDraftNoToken dID = property $ do
  Left err <-
    evalTest
      ( withGetPath ("drafts/" <> T.show dID)
      )
      id
  err `shouldSatisfy` isUnathorizedError

propGetDraftWrongToken :: Entity Draft (Front Display) -> User (Front Display) -> Token -> Property
propGetDraftWrongToken de@(Entity dID draft) u t = property $ do
  let uID = entityID . user . entity . author $ unDraft draft
  Left err <-
    evalTest
      ( withGetPath ("drafts/" <> T.show dID)
          . withToken t
      )
      ( withGetEntities @(Entity User (Front Display)) [Entity uID u]
      )
  err `shouldSatisfy` isEntityNotFoundError

propGetDrafts :: [Entity Draft (Front Display)] -> Token -> Property
propGetDrafts drafts t = property $ do
  res <-
    evalTest
      ( withGetPath "drafts"
          . withToken t
      )
      (withGetEntities @(Entity Draft (Front Display)) drafts)
  res `shouldBe` Right (ResJSON $ encode $ take testPaginationConstant drafts)

propGetDraftsArbitrary :: [Entity Draft (Front Display)] -> Token -> Int -> Int -> Property
propGetDraftsArbitrary drafts t limit offset = property $ do
  res <-
    evalTest
      ( withGetPath "drafts"
          . withToken t
          . withLimit limit
          . withOffset offset
      )
      (withGetEntities @(Entity Draft (Front Display)) drafts)
  res `shouldBe` Right (ResJSON $ encode $ take (min limit testPaginationConstant) $ drop offset drafts)

propGetDraftsNoToken :: Property
propGetDraftsNoToken = property $ do
  Left err <-
    evalTest
      (withGetPath "drafts")
      id
  err `shouldSatisfy` isUnathorizedError

putSpec :: Spec
putSpec = describe "PUT" $ do
  it "It actually updates draft when all is ok" $
    property propPutDraft
  it "Throws an appropriate error when there is no draft with that ID" $
    property propPutDraftDoesntExists
  it "Throws an appropriate error when request body is unparsable" $
    property propPutDraftUnparsable
  it "Throws an appropriate error when no token is provided" $
    property propPutDraftNoToken
  it "Throws an appropriate error when wrong token is provided" $
    property propPutDraftWrongToken

propPutDraft :: Entity Draft (Front Display) -> Draft (Front Update) -> User (Front Display) -> Token -> Property
propPutDraft de@(Entity dID draft) du u t = property $ do
  let uID = entityID . user . entity . author $ unDraft draft
  res <-
    evalTest
      ( withPutPath ("drafts/" <> T.show dID)
          . withBody du
          . withToken t
      )
      ( withGetEntities @(Entity User (Front Display)) [Entity uID u]
          . withGetEntities @(Entity Draft (Front Display)) [de]
      )
  res `shouldBe` Right (ResText "Successfuly updated.")

propPutDraftDoesntExists :: Entity Draft (Front Display) -> Draft (Front Update) -> User (Front Display) -> Token -> Property
propPutDraftDoesntExists de@(Entity dID draft) du u t = property $ do
  let uID = entityID . user . entity . author $ unDraft draft
  Left err <-
    evalTest
      ( withPutPath ("drafts/" <> T.show dID)
          . withBody du
          . withToken t
      )
      ( withGetEntities @(Entity User (Front Display)) [Entity uID u]
      )
  err `shouldSatisfy` isEntityNotFoundError

propPutDraftUnparsable :: Entity Draft (Front Display) -> BL.ByteString -> User (Front Display) -> Token -> Property
propPutDraftUnparsable de@(Entity dID draft) bl u t =
  property $
    isLeft (eitherDecode @(Draft (Front Update)) bl) ==> do
      let uID = entityID . user . entity . author $ unDraft draft
      Left err <-
        evalTest
          ( withPutPath ("drafts/" <> T.show dID)
              . withBLBody bl
              . withToken t
          )
          ( withGetEntities @(Entity User (Front Display)) [Entity uID u]
              . withGetEntities @(Entity Draft (Front Display)) [de]
          )
      err `shouldSatisfy` isParsingError

propPutDraftNoToken :: ID (Draft (Front Update)) -> Property
propPutDraftNoToken dID = property $ do
  Left err <-
    evalTest
      ( withPutPath ("drafts/" <> T.show dID)
      )
      id
  err `shouldSatisfy` isUnathorizedError

propPutDraftWrongToken :: Entity Draft (Front Display) -> Draft (Front Update) -> Token -> Property
propPutDraftWrongToken de@(Entity dID draft) du t = property $ do
  let uID = entityID . user . entity . author $ unDraft draft
  Left err <-
    evalTest
      ( withPutPath ("drafts/" <> T.show dID)
          . withBody du
          . withToken t
      )
      ( withGetEntities @(Entity Draft (Front Display)) [de]
      )
  err `shouldSatisfy` isEntityNotFoundError

deleteSpec :: Spec
deleteSpec = describe "DELETE" $ do
  it "Actually deletes draft from database when all is ok" $
    property propDeleteDraft
  it "Throws an appropriate error when there is no draft with that ID" $
    property propDeleteDraftDoesntExists
  it "Throws an appropriate error when no token is provided" $
    property propDeleteDraftNoToken
  it "Throws an appropriate error when wrong token is provided" $
    property propDeleteDraftWrongToken

propDeleteDraft :: Entity Draft (Front Display) -> User (Front Display) -> Token -> Property
propDeleteDraft de@(Entity dID draft) u t = property $ do
  let uID = entityID . user . entity . author $ unDraft draft
  res <-
    evalTest
      ( withDeletePath ("drafts/" <> T.show dID)
          . withToken t
      )
      ( withGetEntities @(Entity User (Front Display)) [Entity uID u]
          . withGetEntities @(Entity Draft (Front Display)) [de]
      )
  res `shouldBe` Right (ResText "Successfuly deleted.")

propDeleteDraftDoesntExists :: Entity Draft (Front Display) -> User (Front Display) -> Token -> Property
propDeleteDraftDoesntExists de@(Entity dID draft) u t = property $ do
  let uID = entityID . user . entity . author $ unDraft draft
  Left err <-
    evalTest
      ( withDeletePath ("drafts/" <> T.show dID)
          . withToken t
      )
      ( withGetEntities @(Entity User (Front Display)) [Entity uID u]
      )
  err `shouldSatisfy` isEntityNotFoundError

propDeleteDraftNoToken :: ID (Draft Delete) -> Property
propDeleteDraftNoToken dID = property $ do
  Left err <-
    evalTest
      (withDeletePath $ "drafts/" <> T.show dID)
      id
  err `shouldSatisfy` isUnathorizedError

propDeleteDraftWrongToken :: ID (Draft Delete) -> Token -> Property
propDeleteDraftWrongToken dID t = property $ do
  Left err <-
    evalTest
      (withDeletePath ("drafts/" <> T.show dID) . withToken t)
      id
  err `shouldSatisfy` isEntityNotFoundError

publishSpec :: Spec
publishSpec = describe "PUBLISH" $ do
  it "Actually publish draft when all is ok" $
    property propPublishDraft
  it "Throws an appropriate error when there is no draft with that ID" $
    property propPublishDraftDoesntExists
  it "Throws an appropriate error when no token is provided" $
    property propPublishDraftNoToken
  it "Throws an appropriate error when wrong token is provided" $
    property propPublishDraftWrongToken

propPublishDraft :: Entity Draft (Front Display) -> User (Front Display) -> Token -> Property
propPublishDraft de@(Entity dID draft) u t = property $ do
  let uID = entityID . user . entity . author $ unDraft draft
  res <-
    evalTest
      ( withPublishPath ("drafts/" <> T.show dID)
          . withToken t
      )
      ( withGetEntities @(Entity User (Front Display)) [Entity uID u]
          . withGetEntities @(Entity Draft (Front Display)) [de]
      )
  res `shouldBe` Right (ResText "Successfuly published.")

propPublishDraftDoesntExists :: Entity Draft (Front Display) -> User (Front Display) -> Token -> Property
propPublishDraftDoesntExists de@(Entity dID draft) u t = property $ do
  let uID = entityID . user . entity . author $ unDraft draft
  Left err <-
    evalTest
      ( withPublishPath ("drafts/" <> T.show dID)
          . withToken t
      )
      ( withGetEntities @(Entity User (Front Display)) [Entity uID u]
      )
  err `shouldSatisfy` isEntityNotFoundError

propPublishDraftNoToken :: ID (Draft Publish) -> Property
propPublishDraftNoToken dID = property $ do
  Left err <-
    evalTest
      ( withPublishPath ("drafts/" <> T.show dID)
      )
      id
  err `shouldSatisfy` isUnathorizedError

propPublishDraftWrongToken :: Entity Draft (Front Display) -> User (Front Display) -> Token -> Property
propPublishDraftWrongToken de@(Entity dID draft) u t = property $ do
  let uID = entityID . user . entity . author $ unDraft draft
  Left err <-
    evalTest
      ( withPublishPath ("drafts/" <> T.show dID)
          . withToken t
      )
      ( withGetEntities @(Entity Draft (Front Display)) [de]
      )
  err `shouldSatisfy` isEntityNotFoundError

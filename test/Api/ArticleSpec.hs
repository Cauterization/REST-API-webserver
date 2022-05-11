{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Api.ArticleSpec where

import Api.Article
import App.Result
import App.ResultJSON
import App.Types
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.Coerce
import Data.Data (Typeable)
import Data.Either
import Data.Function
import Data.IntMap qualified as IM
import Data.Kind (Type)
import Data.List
import Data.Time qualified as Time
import Entity.Article
import Entity.Author
import Entity.Category
import Entity.Internal
import Entity.Tag
import Entity.User
import Extended.Text (Text)
import Extended.Text qualified as T
import HKD.HKD
import Helpers.App
import Helpers.Article
import Helpers.ArticleDB
import Helpers.Author
import Helpers.Database
import Helpers.Entity
import Helpers.GenericProps
import Helpers.Internal
import Helpers.Monad
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  getSpec

getSpec :: Spec
getSpec = describe "GET" $ do
  context "many" $ do
    it "It allows to get list of articles" $
      property propGetArticles

    it "Allows to select arbitrary limit & offset" $
      property propGetArtcilesLimitOffset

  context "supports filtering by fields:" $ do
    it "created date" $
      property propGetArticlesCrAt

    it "created date (lower than)" $
      property propGetArticlesCrAtLt

    it "created date (greater than)" $
      property propGetArticlesCrAtGt

    it "title" $
      property propGetArticlesTitle

    it "conent" $
      property propGetArticlesContent

    it "author login" $
      property propGetArticlesLogin

    it "substring occurence" $
      property propGetArticlesSubstr

    it "category id" $
      property propGetArticlesCategory

    it "tag id" $
      property propGetArticlesTag

    it "tag id (any from list)" $
      property propGetArticlesTagIn

    it "tag id (all from list)" $
      property propGetArticlesTagAll

  context "single" $ do
    it "When all is ok it allows to get an article by its own ID" $
      property propGetArticle

    it "Throws error when article with this ID doesn't exists" $
      property $ propGetEntityDoesntExists @Article "articles"

propGetArticles :: TDB Article -> Property
propGetArticles dbA =
  property $
    conditions ==> do
      Right (ResJSON j) <-
        evalTest
          (withGetPath "articles")
          (withDatabase @Article dbA)
      let Right ref' = runTestMonadNoMods $ toJSONResult $ ref
      j `shouldBe` encode ref'
  where
    conditions = IM.size dbA < testPaginationConstant
    ref = map (\(a, b) -> Entity (ID a) (fromDisplay @(Article (Front Display)) b)) $ IM.toList dbA

propGetArtcilesLimitOffset :: TDB Article -> Int -> Int -> Property
propGetArtcilesLimitOffset dbA limit offset = property $ do
  Right (ResJSON j) <-
    evalTest
      ( withGetPath "articles"
          . withLimit limit
          . withOffset offset
      )
      (withDatabase @Article dbA)
  let Right ref' = runTestMonadNoMods $ toJSONResult $ ref
  j `shouldBe` encode ref'
  where
    ref = paginate $ map (\(a, b) -> Entity (ID a) (fromDisplay @(Article (Front Display)) b)) $ IM.toList dbA
    paginate = take (min limit testPaginationConstant) . drop offset

takeMax :: [a] -> [a]
takeMax = take testPaginationConstant

propGetArticlesCrAt :: TDB Article -> Date -> Property
propGetArticlesCrAt dbA date = property $ do
  Right (ResJSON j) <-
    evalTest
      ( withGetPath "articles"
          . withParam "crAt" date
      )
      (withDatabase @Article dbA)
  let Right ref' = runTestMonadNoMods $ toJSONResult $ ref
  j `shouldBe` encode ref'
  where
    ref = doFiltering $ map (\(a, b) -> Entity (ID a) (fromDisplay @(Article (Front Display)) b)) $ IM.toList dbA
    doFiltering = takeMax . filter ((== date) . created . entity)

propGetArticlesCrAtLt :: TDB Article -> Date -> Property
propGetArticlesCrAtLt dbA date = property $ do
  Right (ResJSON j) <-
    evalTest
      ( withGetPath "articles"
          . withParam "crAtLt" date
      )
      (withDatabase @Article dbA)
  let Right ref' = runTestMonadNoMods $ toJSONResult $ ref
  j `shouldBe` encode ref'
  where
    ref = doFiltering $ map (\(a, b) -> Entity (ID a) (fromDisplay @(Article (Front Display)) b)) $ IM.toList dbA
    doFiltering = takeMax . filter ((<= date) . created . entity)

propGetArticlesCrAtGt :: TDB Article -> Date -> Property
propGetArticlesCrAtGt dbA date = property $ do
  Right (ResJSON j) <-
    evalTest
      ( withGetPath "articles"
          . withParam "crAtGt" date
      )
      (withDatabase @Article dbA)
  let Right ref' = runTestMonadNoMods $ toJSONResult $ ref
  j `shouldBe` encode ref'
  where
    ref = doFiltering $ map (\(a, b) -> Entity (ID a) (fromDisplay @(Article (Front Display)) b)) $ IM.toList dbA
    doFiltering = takeMax . filter ((>= date) . created . entity)

propGetArticlesTitle :: TDB Article -> Text -> Property
propGetArticlesTitle dbA aTitle = property $ do
  Right (ResJSON j) <-
    evalTest
      ( withGetPath "articles"
          . withParam "title" aTitle
      )
      (withDatabase @Article dbA)
  let Right ref' = runTestMonadNoMods $ toJSONResult $ ref
  j `shouldBe` encode ref'
  where
    ref = doFiltering $ map (\(a, b) -> Entity (ID a) (fromDisplay @(Article (Front Display)) b)) $ IM.toList dbA
    doFiltering = takeMax . filter ((== aTitle) . title . entity)

propGetArticlesContent :: TDB Article -> Text -> Property
propGetArticlesContent dbA aContent = property $ do
  Right (ResJSON j) <-
    evalTest
      ( withGetPath "articles"
          . withParam "content" aContent
      )
      (withDatabase @Article dbA)
  let Right ref' = runTestMonadNoMods $ toJSONResult $ ref
  j `shouldBe` encode ref'
  where
    ref = doFiltering $ map (\(a, b) -> Entity (ID a) (fromDisplay @(Article (Front Display)) b)) $ IM.toList dbA
    doFiltering = takeMax . filter ((aContent `T.isInfixOf`) . content . entity)

propGetArticlesLogin :: TDB Article -> Text -> Property
propGetArticlesLogin dbA alogin = property $ do
  Right (ResJSON j) <-
    evalTest
      ( withGetPath "articles"
          . withParam "author_login" alogin
      )
      (withDatabase @Article dbA)
  let Right ref' = runTestMonadNoMods $ toJSONResult $ ref
  j `shouldBe` encode ref'
  where
    ref = doFiltering $ map (\(a, b) -> Entity (ID a) (fromDisplay @(Article (Front Display)) b)) $ IM.toList dbA
    doFiltering = takeMax . filter ((== alogin) . login . entity . user . entity . author . entity)

propGetArticlesSubstr :: TDB Article -> Text -> Property
propGetArticlesSubstr dbA substr = property $ do
  Right (ResJSON j) <-
    evalTest
      ( withGetPath "articles"
          . withParam "substring" substr
      )
      (withDatabase @Article dbA)
  let Right ref' = runTestMonadNoMods $ toJSONResult $ ref
  j `shouldBe` encode ref'
  where
    ref = doFiltering $ map (\(a, b) -> Entity (ID a) (fromDisplay @(Article (Front Display)) b)) $ IM.toList dbA
    doFiltering = takeMax . filter (isSubstr . entity)
    isSubstr Article {..} =
      let l = login $ entity $ user $ entity $ author
          c = unCatName $ name $ entity $ category
          cs = map unCatName $ parent $ entity category
          ts = map (tag . entity) tags
       in any (substr `T.isInfixOf`) ([content, l, c] <> cs <> ts)

propGetArticlesCategory :: TDB Article -> Int -> Property
propGetArticlesCategory dbA catID = property $ do
  Right (ResJSON j) <-
    evalTest
      ( withGetPath "articles"
          . withParam "category_id" catID
      )
      (withDatabase @Article dbA)
  let Right ref' = runTestMonadNoMods $ toJSONResult $ ref
  j `shouldBe` encode ref'
  where
    ref = doFiltering $ map (\(a, b) -> Entity (ID a) (fromDisplay @(Article (Front Display)) b)) $ IM.toList dbA
    doFiltering = takeMax . filter ((== ID catID) . entityID . category . entity)

propGetArticlesTag :: TDB Article -> Int -> Property
propGetArticlesTag dbA tagID = property $ do
  Right (ResJSON j) <-
    evalTest
      ( withGetPath "articles"
          . withParam "tag_id" tagID
      )
      (withDatabase @Article dbA)
  let Right ref' = runTestMonadNoMods $ toJSONResult $ ref
  j `shouldBe` encode ref'
  where
    ref = doFiltering $ map (\(a, b) -> Entity (ID a) (fromDisplay @(Article (Front Display)) b)) $ IM.toList dbA
    doFiltering = takeMax . filter ((ID tagID `elem`) . map entityID . tags . entity)

propGetArticlesTagIn :: TDB Article -> [Int] -> Property
propGetArticlesTagIn dbA tagIn = property $ do
  res <-
    evalTest
      ( withGetPath "articles"
          . withParam "tag_in" tagIn
      )
      (withDatabase @Article dbA)
  case res of
    Left err -> error $ show err
    Right (ResJSON j) -> do
      let Right ref' = runTestMonadNoMods $ toJSONResult $ ref
      j `shouldBe` encode ref'
  where
    ref = doFiltering $ map (\(a, b) -> Entity (ID a) (fromDisplay @(Article (Front Display)) b)) $ IM.toList dbA
    doFiltering = takeMax . filter ((any (`elem` (map ID tagIn))) . map entityID . tags . entity)

propGetArticlesTagAll :: TDB Article -> [Int] -> Property
propGetArticlesTagAll dbA tagAll = property $ do
  Right (ResJSON j) <-
    evalTest
      ( withGetPath "articles"
          . withParam "tag_all" tagAll
      )
      (withDatabase @Article dbA)
  let Right ref' = runTestMonadNoMods $ toJSONResult $ ref
  j `shouldBe` encode ref'
  where
    ref = doFiltering $ map (\(a, b) -> Entity (ID a) (fromDisplay @(Article (Front Display)) b)) $ IM.toList dbA
    doFiltering = takeMax . filter ((all (`elem` (map ID tagAll))) . map entityID . tags . entity)

propGetArticle :: TDB Article -> ID (Article Display) -> Property
propGetArticle dbA (ID articleID) =
  property $
    articleID `elem` IM.keys dbA ==> do
      res <-
        evalTest
          (withGetPath $ "articles/" <> T.show articleID)
          (withDatabase @Article dbA)
      case res of
        Left err -> error $ show err
        Right (ResJSON j) -> do
          j ^? key "title" . _String
            `shouldBe` title <$> IM.lookup articleID dbA

{-# LANGUAGE ViewPatterns #-}

module Api.ArticleSpec where

import Api.Article
import App.Types
import App.Result
import App.ResultJSON

import Control.Lens

import Data.Aeson
import Data.Aeson.Lens
import Data.Either
import Data.IntMap qualified as IM
import Data.Coerce
import Data.Kind (Type)
import Data.List

import Entity.Author
import Entity.Article
import Entity.User
import Entity.Internal
import Extended.Text qualified as T
import Extended.Text (Text)


import Helpers.Author
import Helpers.Article
import Helpers.ArticleDB
import Helpers.Internal
import Helpers.Entity
import Helpers.GenericProps
import Test.Hspec 
import Test.QuickCheck
import Helpers.Monad
import Helpers.App
import Helpers.Database
import HKD.HKD
import Data.Data (Typeable)



spec :: Spec
spec = do
    pure ()
    getSpec


getSpec :: Spec
getSpec = describe "GET" $ do

--     context "many" $ do

--         it "When all is ok it gets list of authors"
--             $ property $ propGetEntities @Author "authors"

--         it "This list is paginated"
--             $ property $ propGetEntitiesIsPaginated @Author "authors"

--         it "Allows to get various pages of this list"
--             $ property $ propGetEntitiesWithLimitOffset @Author "authors"

    context "single" $ do

        it "When all is ok it allows to get an article by its own ID"
            $ property $ propGetArticle 

        it "Throws error when article with this ID doesn't exists"
            $ property $ propGetEntityDoesntExists @Article "articles"

propGetArticle :: TDB Article -> ID (Article Display) -> Property
propGetArticle dbA (ID articleID) = property $ articleID `elem` IM.keys dbA ==> do
    res <- evalTest
        (withGetPath $ "articles/" <> T.show articleID)
        (withDatabase @Article dbA)
    case res of
        Left err -> error $ show err
        Right (ResJSON j) -> do
            j ^? key "title" . _String 
                `shouldBe` title <$> IM.lookup articleID dbA
  


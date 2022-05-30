{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Api.ArticleSpec where

import Api.Article
import App.Error
import App.Path
import App.Result
import App.Types
import Control.Monad
import Control.Monad.Catch
import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Data
import Data.Either
import Data.Kind
import Database.Internal qualified as Database
import Entity.Internal
import Entity.Article
import Extended.Text (Text)
import Extended.Text qualified as T
import HKD.HKD
import Mocks.Arbitrary
import Mocks.Constant
import Mocks.Entity.Article ()
import Mocks.Predicates
import Mocks.Run
import Mocks.TestMonad
import Mocks.With
import Test.Hspec
import Test.QuickCheck
import Data.Maybe
import Data.List
import Data.Coerce
import qualified Database.EntityFilters as Database

spec :: Spec
spec = describe "Get articles" $ do
  it "supports filtering by specific fields" $ 
    property propGetWithFilters

propGetWithFilters ::
  [Entity Article (Front Display)] ->
  Maybe Date ->
  Maybe Date ->
  Maybe Date ->
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  Maybe [Int] ->
  Maybe [Int] ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Int ->
  Int ->
  Property
propGetWithFilters 
  articles 
  crAt 
  crAtLt 
  crAtGt 
  auLogin 
  catID 
  tagID 
  tagIn 
  tagAll 
  arTitle 
  arContent 
  substr 
  limit
  offset
  = property $ do
  let fs = [ Database.EFPDateOptional crAt
           , Database.EFPDateOptional crAtLt
           , Database.EFPDateOptional crAtGt
           , Database.EFPTextOptional auLogin
           , Database.EFPIntOptional catID
           , Database.EFPIntOptional tagID
           , Database.EFPIntListOptional tagIn
           , Database.EFPIntListOptional tagAll
           , Database.EFPTextOptional arTitle
           , Database.EFPTextOptional arContent
           , Database.EFPTextOptional substr
           , Database.EFPInt (min testPaginationConstant limit)
           , Database.EFPInt offset
           ]
  res <-
    evalTest
      ( withGetPath "articles"
      . withMaybeParam "crAt" crAt
      . withMaybeParam "crAtLt" crAtLt
      . withMaybeParam "crAtGt" crAtGt
      . withMaybeTextParam "author_login" auLogin
      . withMaybeParam "category_id" catID
      . withMaybeParam "tag_id" tagID
      . withMaybeParam "tag_in" tagIn
      . withMaybeParam "tag_all" tagAll
      . withMaybeTextParam "title" arTitle
      . withMaybeTextParam "content" arContent
      . withMaybeTextParam "substring" substr
      . withLimit limit
      . withOffset offset
      )
      (withGetEntities @(Entity Article (Front Display)) articles)
  res `shouldBe` Right (ResJSON $ encode $ applyFilters fs articles)
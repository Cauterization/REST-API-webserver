{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Api.TagSpec where

import App.App
import App.Result
import Data.Aeson
import Data.Map qualified as M
import Entity.Tag
import Extended.Text qualified as T
import HKD.HKD
import Helpers.App
import Helpers.Database
import Helpers.Entity
import Helpers.GenericProps
import Helpers.Internal
import Helpers.Monad
import Helpers.Tag
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  postSpec
  getSpec
  putSpec
  deleteSpec

postSpec :: Spec
postSpec = describe "POST" $ do
  it "When all is ok it posts tag into the database" $
    property $ propPostsEntity @Tag "tags"

  it "Throws error when tag already in the database" $
    property $ propPostsAlreadyExists @Tag "tags"

  it "Throws error when it fails to parse request body" $
    property $ propPostsParsingFail @Tag "tags"

getSpec :: Spec
getSpec = describe "GET" $ do
  context "many" $ do
    it "When all is ok it gets list of tags" $
      property $ propGetEntities @Tag "tags"

    it "This list is paginated" $
      property $ propGetEntitiesIsPaginated @Tag "tags"

    it "Allows to get various pages of this list" $
      property $ propGetEntitiesWithLimitOffset @Tag "tags"

  context "single" $ do
    it "When all is ok it allows to get an tag by its own ID" $
      property $ propGetEntity @Tag "tags"

    it "Throws error when tag with this ID doesn't exists" $
      property $ propGetEntityDoesntExists @Tag "tags"

putSpec :: Spec
putSpec = describe "PUT" $ do
  it "Actually change tag " $
    property $ propPutEntity @Tag "tags"

  it "Throws error when tag with this ID doesn't exists" $
    property $ propPutEntityDoesntExists @Tag "tags"

  it "Throws error when it fails to parse request body" $
    property $ propPutEntityParsingFail @Tag "tags"

deleteSpec :: Spec
deleteSpec = describe "DELETE" $ do
  it "Actually deletes tag from database" $
    property $ propDeleteEntity @Tag "tags"

  it "Throws error when tag with this ID doesn't exists" $
    property $ propDeleteEntityDoesntExists @Tag "tags"

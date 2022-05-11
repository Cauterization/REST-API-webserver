{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Api.CategorySpec where

import App.Result
import App.Types
import Data.Aeson
import Data.Coerce
import Data.Either
import Data.Kind (Type)
import Data.List
import Data.Map qualified as M
import Entity.Category
import Entity.Internal
import Extended.Text (Text)
import Extended.Text qualified as T
import HKD.HKD
import Helpers.App
import Helpers.Category
import Helpers.CategoryDB
import Helpers.Database
import Helpers.Entity
import Helpers.GenericProps
import Helpers.Internal
import Helpers.Monad
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  postSpec
  deleteSpec

postSpec :: Spec
postSpec = describe "POST" $ do
  it "When all is ok it posts category into the database" $
    property $ propPostsEntity @Category "categories"

  it "Throws error when category already in the database" $
    property $ propPostsAlreadyExists @Category "categories"

  it "Throws error when it fails to parse request body" $
    property $ propPostsParsingFail @Category "categories"

deleteSpec :: Spec
deleteSpec = describe "DELETE" $ do
  it "Actually deletes category from database" $
    property $ propDeleteEntity @Category "categories"

  it "Throws error when category with this ID doesn't exists" $
    property $ propDeleteEntityDoesntExists @Category "categories"

-- | Other endpoints rely heavily on Postgres recursive queries so I don't think
-- it makes sense to test them.

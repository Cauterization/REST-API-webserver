module Api.CategorySpec where

import Data.Kind (Type)
import Data.Map qualified as M
import Entity.Category ( Category )
import Extended.Text (Text)
import Extended.Text qualified as T
import Helpers.Category ()
import Helpers.GenericProps
    ( propDeleteEntity,
      propDeleteEntityDoesntExists,
      propPostsAlreadyExists,
      propPostsEntity,
      propPostsParsingFail )
import Test.Hspec ( Spec, describe, it )
import Test.QuickCheck ( Testable(property) )

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

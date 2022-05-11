module Api.AuthorSpec where

import Data.Data (Typeable)
import Data.IntMap qualified as IM
import Data.Kind (Type)
import Entity.Author ( Author )
import Extended.Text (Text)
import Extended.Text qualified as T
import Helpers.Author ()
import Helpers.GenericProps
    ( propDeleteEntity,
      propDeleteEntityDoesntExists,
      propGetEntities,
      propGetEntitiesIsPaginated,
      propGetEntitiesWithLimitOffset,
      propGetEntity,
      propGetEntityDoesntExists,
      propPostsAlreadyExists,
      propPostsEntity,
      propPostsParsingFail,
      propPutEntity,
      propPutEntityDoesntExists,
      propPutEntityParsingFail )
import Test.Hspec ( Spec, context, describe, it )
import Test.QuickCheck ( Testable(property) )

spec :: Spec
spec = do
  postSpec
  getSpec
  putSpec
  deleteSpec

postSpec :: Spec
postSpec = describe "POST" $ do
  it "When all is ok it posts author into the database" $
    property $ propPostsEntity @Author "authors"

  it "Throws error when author already in the database" $
    property $ propPostsAlreadyExists @Author "authors"

  it "Throws error when it fails to parse request body" $
    property $ propPostsParsingFail @Author "authors"

getSpec :: Spec
getSpec = describe "GET" $ do
  context "many" $ do
    it "When all is ok it gets list of authors" $
      property $ propGetEntities @Author "authors"

    it "This list is paginated" $
      property $ propGetEntitiesIsPaginated @Author "authors"

    it "Allows to get various pages of this list" $
      property $ propGetEntitiesWithLimitOffset @Author "authors"

  context "single" $ do
    it "When all is ok it allows to get an author by its own ID" $
      property $ propGetEntity @Author "authors"

    it "Throws error when author with this ID doesn't exists" $
      property $ propGetEntityDoesntExists @Author "authors"

putSpec :: Spec
putSpec = describe "PUT" $ do
  it "Actually change author " $
    property $ propPutEntity @Author "authors"

  it "Throws error when author with this ID doesn't exists" $
    property $ propPutEntityDoesntExists @Author "authors"

  it "Throws error when it fails to parse request body" $
    property $ propPutEntityParsingFail @Author "authors"

deleteSpec :: Spec
deleteSpec = describe "DELETE" $ do
  it "Actually deletes author from database" $
    property $ propDeleteEntity @Author "authors"

  it "Throws error when author with this ID doesn't exists" $
    property $ propDeleteEntityDoesntExists @Author "authors"

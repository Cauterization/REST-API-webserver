module Api.GetSpec where

import App.Result (AppResult (ResJSON))
import App.Types (ID, nameOf, withPluralEnding)
import Data.Aeson (ToJSON, encode)
import Data.ByteString.Lazy qualified as BL
import Data.Data (Typeable)
import Data.Kind (Type)
import Entity.Article (Article)
import Entity.Author (Author)
import Entity.Category
import Entity.Internal (Entity (Entity))
import Entity.Picture (Picture)
import Entity.Tag (Tag)
import Extended.Text qualified as T
import HKD.HKD (Display, Front)
import Mocks.Constant (testPaginationConstant)
import Mocks.Predicates (isEntityNotFoundError)
import Mocks.Run (EnvEndo, evalTest)
import Mocks.TestMonad (TestEntity (withGetEntities))
import Mocks.Utils (mkPathFromID)
import Mocks.With (withGetPath, withLimit, withOffset)
import Test.Hspec
  ( Example (Arg),
    Spec,
    SpecWith,
    context,
    describe,
    it,
    shouldBe,
    shouldSatisfy,
  )
import Test.QuickCheck (Arbitrary, Property, Testable (property))

spec :: Spec
spec = do
  describe "single" $ do
    context "Allows to get entity from database when all is ok" $ do
      testGetSingle @Article
      testGetSingle @Author
      testGetSingle @Category
      testGetSingle @Tag

    context "Throws an appropriate error when there is no entity with that ID" $ do
      testGetSingleDoesntExists @Article
      testGetSingleDoesntExists @Author
      testGetSingleDoesntExists @Category
      testGetSingleDoesntExists @Picture
      testGetSingleDoesntExists @Tag

  describe "many" $ do
    context "Allows to get paginated list of entities when all is ok" $ do
      testGetMany @Article
      testGetMany @Author
      testGetMany @Category
      testGetMany @Tag

    context "Supports arbitrary limit & offset" $ do
      testGetArbitrary @Article
      testGetArbitrary @Author
      testGetArbitrary @Category
      testGetArbitrary @Tag

testGetSingle ::
  forall (e :: Type -> Type).
  ( Typeable e,
    Arbitrary (e (Front Display)),
    Show (e (Front Display)),
    TestEntity (Entity e (Front Display)),
    ToJSON (e (Front Display))
  ) =>
  SpecWith (Arg Property)
testGetSingle = it (nameOf @e) $ property $ propGetSingle @e

testGetSingleDoesntExists ::
  forall (e :: Type -> Type).
  ( Typeable e,
    TestEntity (Entity e (Front Display))
  ) =>
  SpecWith (Arg Property)
testGetSingleDoesntExists = it (nameOf @e) $ property $ propGetSingleDoesntExists @e

propGetSingle ::
  forall (e :: Type -> Type).
  ( Typeable e,
    TestEntity (Entity e (Front Display)),
    ToJSON (e (Front Display))
  ) =>
  ID (e (Front Display)) ->
  e (Front Display) ->
  Property
propGetSingle eID entity = property $ do
  let entityWithID = Entity eID entity
  res <-
    evalTest
      (withGetPath $ mkPathFromID @e eID)
      (withGetEntities @(Entity e (Front Display)) [entityWithID])
  res `shouldBe` Right (ResJSON $ encode entityWithID)

propGetSingleDoesntExists ::
  forall (e :: Type -> Type).
  ( Typeable e,
    TestEntity (Entity e (Front Display))
  ) =>
  ID (e (Front Display)) ->
  Property
propGetSingleDoesntExists eID = property $ do
  Left err <-
    evalTest
      (withGetPath $ mkPathFromID @e eID)
      (withGetEntities @(Entity e (Front Display)) [])
  err `shouldSatisfy` isEntityNotFoundError

withGetManyPath :: forall (e :: Type -> Type). Typeable e => EnvEndo
withGetManyPath = withGetPath $ T.pack (withPluralEnding (nameOf @e))

testGetMany ::
  forall (e :: Type -> Type).
  ( Typeable e,
    Arbitrary (Entity e (Front Display)),
    Show (e (Front Display)),
    TestEntity (Entity e (Front Display)),
    ToJSON (e (Front Display))
  ) =>
  SpecWith (Arg Property)
testGetMany = it (nameOf @e) $ property $ propGetMany @e

propGetMany ::
  forall (e :: Type -> Type).
  ( Typeable e,
    Arbitrary (Entity e (Front Display)),
    TestEntity (Entity e (Front Display)),
    ToJSON (e (Front Display))
  ) =>
  [Entity e (Front Display)] ->
  Property
propGetMany entities = property $ do
  res <-
    evalTest
      (withGetManyPath @e)
      (withGetEntities @(Entity e (Front Display)) entities)
  res `shouldBe` Right (ResJSON $ encode $ take testPaginationConstant entities)

testGetArbitrary ::
  forall (e :: Type -> Type).
  ( Typeable e,
    Arbitrary (Entity e (Front Display)),
    Show (e (Front Display)),
    TestEntity (Entity e (Front Display)),
    ToJSON (e (Front Display))
  ) =>
  SpecWith (Arg Property)
testGetArbitrary = it (nameOf @e) $ property $ propGetManyArbitrary @e

propGetManyArbitrary ::
  forall (e :: Type -> Type).
  ( Typeable e,
    Arbitrary (Entity e (Front Display)),
    TestEntity (Entity e (Front Display)),
    ToJSON (e (Front Display))
  ) =>
  Int ->
  Int ->
  [Entity e (Front Display)] ->
  Property
propGetManyArbitrary limit offset entities = property $ do
  res <-
    evalTest
      (withGetManyPath @e . withLimit limit . withOffset offset)
      (withGetEntities @(Entity e (Front Display)) entities)
  res `shouldBe` Right (ResJSON $ encode $ take (min limit testPaginationConstant) $ drop offset entities)

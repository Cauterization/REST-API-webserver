{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Api.GetSpec where

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
import Entity.Article
import Entity.Author
import Entity.Category
import Entity.Picture
import Entity.Internal
import Entity.Tag
import Extended.Text qualified as T
import HKD.HKD
import Mocks.Arbitrary
import Mocks.Constant
import Mocks.Predicates
import Mocks.Run
import Mocks.TestMonad
import Mocks.Utils
import Mocks.With
import Test.Hspec
import Test.QuickCheck

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

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Api.DeleteSpec where

import App.Error
import App.Result
import App.Types
import Data.Data
import Data.Kind
import Entity.Author
import Entity.Tag
import Extended.Text qualified as T
import HKD.HKD
import Mocks.Arbitrary
import Mocks.Predicates
import Mocks.Run
import Mocks.TestMonad
import Mocks.Utils
import Mocks.With
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "Delete API" $ do
  context "Actually deletes entity from database when all is ok" $ do
    testDelete @Author
    testDelete @Tag

  context "Throws an appropriate error when there is no entity with that ID" $ do
    testDeleteDoesntExists @Author
    testDeleteDoesntExists @Tag

testDelete :: forall (e :: Type -> Type). Typeable e => SpecWith (Arg Property)
testDelete = it (nameOf @e) $ property $ propDelete @e

testDeleteDoesntExists :: forall (e :: Type -> Type). Typeable e => SpecWith (Arg Property)
testDeleteDoesntExists = it (nameOf @e) $ property $ propDeleteDoesntExists @e

propDelete :: forall (e :: Type -> Type). Typeable e => ID (e Delete) -> Property
propDelete eID = property $ do
  res <-
    evalTest
      (withDeletePath $ mkPathFromID @e eID)
      id
  res `shouldBe` Right (ResText "Successfuly deleted.")

propDeleteDoesntExists :: forall (e :: Type -> Type). Typeable e => ID (e Delete) -> Property
propDeleteDoesntExists eID = property $ do
  Left err <-
    evalTest
      (withDeletePath $ mkPathFromID @e eID)
      withFailedDelete
  err `shouldSatisfy` isEntityNotFoundError

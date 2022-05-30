{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module Api.DeleteSpec where

import App.Result (AppResult (ResText))
import App.Types (ID, nameOf)
import Data.Data (Typeable)
import Data.Kind (Type)
import Entity.Author (Author)
import Entity.Category (Category)
import Entity.Picture (Picture)
import Entity.Tag (Tag)
import Entity.User (User)
import Extended.Text qualified as T
import HKD.HKD (Delete)
import Mocks.Predicates (isEntityNotFoundError)
import Mocks.Run (evalTest)
import Mocks.Utils (mkPathFromID)
import Mocks.With (withDeletePath, withFailedDelete)
import Test.Hspec
  ( Example (Arg),
    Spec,
    SpecWith,
    describe,
    it,
    shouldBe,
    shouldSatisfy,
  )
import Test.QuickCheck (Property, Testable (property))

spec :: Spec
spec = do
  describe "Actually deletes entity from database when all is ok" $ do
    testDelete @Author
    testDelete @Category
    testDelete @Picture
    testDelete @Tag
    testDelete @User

  describe "Throws an appropriate error when there is no entity with that ID" $ do
    testDeleteDoesntExists @Author
    testDeleteDoesntExists @Category
    testDeleteDoesntExists @Picture
    testDeleteDoesntExists @Tag
    testDeleteDoesntExists @User

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

module Api.PutSpec where

import App.Result (AppResult (ResText))
import App.Types (ID, nameOf)
import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import Data.ByteString.Lazy qualified as BL
import Data.Data (Typeable)
import Data.Either (isLeft)
import Data.Kind (Type)
import Entity.Author (Author)
import Entity.Category (Category)
import Entity.Tag (Tag)
import HKD.HKD (Front, Update)
import Mocks.Predicates (isEntityNotFoundError, isParsingError)
import Mocks.Run (evalTest)
import Mocks.Utils (mkPathFromID)
import Mocks.With
  ( withBLBody,
    withBody,
    withFailedPut,
    withPutPath,
  )
import Test.Hspec
  ( Example (Arg),
    Spec,
    SpecWith,
    describe,
    it,
    shouldBe,
    shouldSatisfy,
  )
import Test.QuickCheck
  ( Arbitrary,
    Property,
    Testable (property),
    (==>),
  )

spec :: Spec
spec = do
  describe "It actually updates entity when all is ok" $ do
    testPut @Author
    testPut @Tag

  describe "Throws an appropriate error when there is no entity with that ID" $ do
    testPutDoesntExists @Author
    testPutDoesntExists @Tag

  describe "Throws an appropriate error when request body is unparsable" $ do
    testPutUnparsable @Author
    testPutUnparsable @Category
    testPutUnparsable @Tag

testPut ::
  forall (e :: Type -> Type).
  ( Typeable e,
    ToJSON (e (Front Update)),
    Arbitrary (e (Front Update)),
    Show (e (Front Update))
  ) =>
  SpecWith (Arg Property)
testPut = it (nameOf @e) $ property $ propPut @e

testPutDoesntExists ::
  forall (e :: Type -> Type).
  ( Typeable e,
    ToJSON (e (Front Update)),
    Arbitrary (e (Front Update)),
    Show (e (Front Update))
  ) =>
  SpecWith (Arg Property)
testPutDoesntExists = it (nameOf @e) $ property $ propPutDoesntExists @e

testPutUnparsable ::
  forall (e :: Type -> Type).
  ( Typeable e,
    FromJSON (e (Front Update))
  ) =>
  SpecWith (Arg Property)
testPutUnparsable = it (nameOf @e) $ property $ propPutUnparsable @e

propPut ::
  forall (e :: Type -> Type).
  ( Typeable e,
    ToJSON (e (Front Update))
  ) =>
  e (Front Update) ->
  ID (e Update) ->
  Property
propPut eFU eID = property $ do
  res <-
    evalTest
      (withPutPath (mkPathFromID @e eID) . withBody eFU)
      id
  res `shouldBe` Right (ResText "Successfuly updated.")

propPutDoesntExists ::
  forall (e :: Type -> Type).
  ( Typeable e,
    ToJSON (e (Front Update))
  ) =>
  e (Front Update) ->
  ID (e Update) ->
  Property
propPutDoesntExists eFU eID = property $ do
  Left err <-
    evalTest
      (withPutPath (mkPathFromID @e eID) . withBody eFU)
      withFailedPut
  err `shouldSatisfy` isEntityNotFoundError

propPutUnparsable ::
  forall (e :: Type -> Type).
  ( Typeable e,
    FromJSON (e (Front Update))
  ) =>
  BL.ByteString ->
  ID (e Update) ->
  Property
propPutUnparsable bl eID =
  property $
    isLeft (eitherDecode @(e (Front Update)) bl) ==> do
      Left err <-
        evalTest
          (withPutPath (mkPathFromID @e eID) . withBLBody bl)
          id
      err `shouldSatisfy` isParsingError

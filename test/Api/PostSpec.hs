{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Api.PostSpec where

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
import Entity.Author
import Entity.Category
import Entity.Tag
import Entity.User
import Extended.Text qualified as T
import HKD.HKD
import Mocks.Arbitrary
import Mocks.Predicates
import Mocks.Run
import Mocks.TestMonad
import Mocks.With
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Actually posts entity into database when all is ok" $ do
    testPost @Author
    testPost @Category
    testPost @Tag

  describe "Throws an appropriate error when request body is unparsable" $ do
    testPostUnparsable @Author
    testPostUnparsable @Category
    testPostUnparsable @Tag
    testPostUnparsable @User

  describe "Throws an appropriate error when this entity is already in database" $ do
    testPostAlreadyExists @Author
    testPostAlreadyExists @Category
    testPostAlreadyExists @Tag
    testPostAlreadyExists @User

testPost ::
  forall (e :: Type -> Type).
  ( Typeable e,
    ToJSON (e (Front Create)),
    Arbitrary (e (Front Create)),
    Show (e (Front Create))
  ) =>
  SpecWith (Arg Property)
testPost = it (nameOf @e) $ property $ propPost @e

testPostUnparsable ::
  forall (e :: Type -> Type).
  ( Typeable e,
    FromJSON (e (Front Create))
  ) =>
  SpecWith (Arg Property)
testPostUnparsable = it (nameOf @e) $ property $ propPostUnparsable @e

testPostAlreadyExists ::
  forall (e :: Type -> Type).
  ( Typeable e,
    ToJSON (e (Front Create)),
    Arbitrary (e (Front Create)),
    Show (e (Front Create))
  ) =>
  SpecWith (Arg Property)
testPostAlreadyExists = it (nameOf @e) $ property $ propPostAlreadyExists @e

propPost :: forall (e :: Type -> Type). (Typeable e, ToJSON (e (Front Create))) => e (Front Create) -> Property
propPost entity = property $ do
  res <-
    evalTest
      (withEPostPath @e . withBody @(e (Front Create)) entity)
      id
  res `shouldBe` Right (ResText $ T.show defaultPostResult)

propPostUnparsable ::
  forall (e :: Type -> Type).
  ( Typeable e,
    FromJSON (e (Front Create))
  ) =>
  BL.ByteString ->
  Property
propPostUnparsable bl =
  property $
    isLeft (eitherDecode @(e (Front Create)) bl) ==> do
      Left err <-
        evalTest
          (withEPostPath @e . withBLBody bl)
          id
      err `shouldSatisfy` isParsingError

propPostAlreadyExists :: forall (e :: Type -> Type). 
  ( Typeable e
  , ToJSON (e (Front Create))
  ) => 
  e (Front Create) -> Property
propPostAlreadyExists entity = property $ do
  Left err <-
    evalTest
      (withEPostPath @e . withBody @(e (Front Create)) entity)
      withAlreadyExistsPosts
  err `shouldSatisfy` isAlreadyExistsError
  where
    withAlreadyExistsPosts TestState {..} = TestState {postResult = throwAE, ..}
    throwAE = throwM $ Database.AlreadyExists ""

withEPostPath :: forall (e :: Type -> Type). Typeable e => EnvEndo
withEPostPath = withPostPath $ T.pack (withPluralEnding (nameOf @e))

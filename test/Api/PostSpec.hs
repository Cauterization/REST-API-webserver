{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module Api.PostSpec where

import App.Result (AppResult (ResText))
import App.Types (nameOf, withPluralEnding)
import Control.Monad.Catch (MonadThrow (throwM))
import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import Data.ByteString.Lazy qualified as BL
import Data.Data (Typeable)
import Data.Either (isLeft)
import Data.Kind (Type)
import Database.Internal qualified as Database
import Entity.Author (Author)
import Entity.Category (Category)
import Entity.Draft (Draft)
import Entity.Tag (Tag)
import Entity.User (User)
import Extended.Text qualified as T
import HKD.HKD (Create, Front)
import Mocks.Predicates (isAlreadyExistsError, isParsingError)
import Mocks.Run (EnvEndo, evalTest)
import Mocks.TestMonad
  ( TestState
      ( TestState,
        deleteResult,
        filters,
        getArticles,
        getAuthors,
        getCategories,
        getCategoriesID,
        getDrafts,
        getEntityPictures,
        getPictures,
        getTags,
        getUsers,
        getUsersDisplay,
        postResult,
        putResult,
        userToken
      ),
    defaultPostResult,
  )
import Mocks.With (withBLBody, withBody, withPostPath)
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
  describe "Actually posts entity into database when all is ok" $ do
    testPost @Author
    testPost @Category
    testPost @Tag

  describe "Throws an appropriate error when request body is unparsable" $ do
    testPostUnparsable @Author
    testPostUnparsable @Draft
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

propPostAlreadyExists ::
  forall (e :: Type -> Type).
  ( Typeable e,
    ToJSON (e (Front Create))
  ) =>
  e (Front Create) ->
  Property
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

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Api.UserSpec where

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
import Entity.Tag
import Entity.User
import Extended.Text qualified as T
import HKD.HKD
import Mocks.Arbitrary
import Mocks.Constant
import Mocks.Predicates
import Mocks.Run
import Mocks.TestMonad
import Mocks.With
import Test.Hspec
import Test.QuickCheck


spec :: Spec
spec = describe "User API" $ do
  it "Actually posts user into database when all is ok" $ property propPostUser

propPostUser :: User (Front Create) -> Property
propPostUser user = property $ do
    res <- evalTest
      (withPostPath "users" . withBody user)
      id
    res `shouldBe` Right (ResJSON $ encode (defaultPostResult, testTokenConstant))

-- testPost ::
--   forall (e :: Type -> Type).
--   ( Typeable e,
--     ToJSON (e Create),
--     Arbitrary (e Create),
--     Show (e Create)
--   ) =>
--   SpecWith (Arg Property)
-- testPost = it (nameOf @e) $ property $ propPost @e

-- testPostUnparsable ::
--   forall (e :: Type -> Type).
--   ( Typeable e,
--     FromJSON (e (Front Create))
--   ) =>
--   SpecWith (Arg Property)
-- testPostUnparsable = it (nameOf @e) $ property $ propPostUnparsable @e

-- testPostAlreadyExists ::
--   forall (e :: Type -> Type).
--   ( Typeable e,
--     ToJSON (e Create),
--     Arbitrary (e Create),
--     Show (e Create)
--   ) =>
--   SpecWith (Arg Property)
-- testPostAlreadyExists = it (nameOf @e) $ property $ propPostAlreadyExists @e

-- propPost :: forall (e :: Type -> Type). (Typeable e, ToJSON (e Create)) => e Create -> Property
-- propPost entity = property $ do
--   res <-
--     evalTest
--       (withEPostPath @e . withBody @(e Create) entity)
--       id
--   res `shouldBe` Right (ResText $ T.show defaultPostResult)

-- propPostUnparsable ::
--   forall (e :: Type -> Type).
--   ( Typeable e,
--     FromJSON (e (Front Create))
--   ) =>
--   BL.ByteString ->
--   Property
-- propPostUnparsable bl =
--   property $
--     isLeft (eitherDecode @(e (Front Create)) bl) ==> do
--       Left err <-
--         evalTest
--           (withEPostPath @e . withBLBody bl)
--           id
--       err `shouldSatisfy` isParsingError

-- propPostAlreadyExists :: forall (e :: Type -> Type). (Typeable e, ToJSON (e Create)) => e Create -> Property
-- propPostAlreadyExists entity = property $ do
--   Left err <-
--     evalTest
--       (withEPostPath @e . withBody @(e Create) entity)
--       withAlreadyExistsPosts
--   err `shouldSatisfy` isAlreadyExistsError
--   where
--     withAlreadyExistsPosts TestState {..} = TestState {postResult = throwAE, ..}
--     throwAE = throwM $ Database.AlreadyExists ""

-- withEPostPath :: forall (e :: Type -> Type). Typeable e => EnvEndo
-- withEPostPath = withPostPath $ T.pack (withPluralEnding (nameOf @e))

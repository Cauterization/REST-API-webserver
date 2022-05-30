{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Mocks.Entity.User where

import App.Types
import Control.Monad.State
import Data.Aeson
import Entity.Internal
import Entity.User
import HKD.HKD
import Mocks.Arbitrary
import Mocks.TestMonad
import Mocks.Utils
import Test.Hspec
import Test.QuickCheck
import Data.Maybe

instance ToJSON (User (Front Create)) where
  toJSON =
    genericToJSON
      defaultOptions
        { omitNothingFields = True,
          fieldLabelModifier = camelTo2 '_'
        }

deriving instance ToJSON (User Auth)

-- instance ToJSON (User Create) where
--   toJSON =
--     genericToJSON
--       defaultOptions
--         { omitNothingFields = True,
--           fieldLabelModifier = camelTo2 '_'
--         }

instance Arbitrary (User (Front Create)) where
  arbitrary = do
    firstName <- arbitrary
    lastName <- arbitrary
    login <- arbitrary
    token <- arbitrary
    password <- arbitrary
    registered <- arbitrary
    admin <- arbitrary
    pure User{..}

instance Arbitrary (User Create) where
  arbitrary = do
    firstName <- arbitrary
    lastName <- arbitrary
    login <- arbitrary
    token <- arbitrary
    password <- arbitrary
    registered <- arbitrary
    admin <- arbitrary
    pure User{..}

instance Arbitrary (User (Front Display)) where
  arbitrary = do
    firstName <- arbitrary
    lastName <- arbitrary
    login <- arbitrary
    token <- arbitrary
    password <- arbitrary
    registered <- arbitrary
    admin <- arbitrary
    pure User{..}

instance Arbitrary (User Display) where
  arbitrary = do
    firstName <- arbitrary
    lastName <- arbitrary
    login <- arbitrary
    token <- arbitrary
    password <- arbitrary
    registered <- arbitrary
    admin <- arbitrary
    pure User{..}

instance Arbitrary (User Auth) where
  arbitrary = do
    firstName <- arbitrary
    lastName <- arbitrary
    login <- arbitrary
    token <- arbitrary
    password <- arbitrary
    registered <- arbitrary
    admin <- arbitrary
    pure User{..}
  

instance TestEntity (User Create)

instance TestEntity [Token] where
  addToState [t] = modify $ \TestState{..} -> TestState{userToken = Just t, ..}
  addToState _ = undefined

instance TestEntity (Entity User Display) where
  getFromTestDatabase _ = join $ gets getUsersDisplay
  withGetEntities users TestState{..} = TestState{getUsersDisplay = pure users, ..}

instance TestEntity (Entity User (Front Display)) where
  getFromState = join $ gets getUsers
  withGetEntities users TestState {..} = TestState {getUsers = pure users, ..}



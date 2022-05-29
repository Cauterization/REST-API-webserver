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

instance ToJSON (User (Front Create)) where
  toJSON =
    genericToJSON
      defaultOptions
        { omitNothingFields = True,
          fieldLabelModifier = camelTo2 '_'
        }

instance ToJSON (User Create) where
  toJSON =
    genericToJSON
      defaultOptions
        { omitNothingFields = True,
          fieldLabelModifier = camelTo2 '_'
        }

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

instance TestEntity (User Create)

-- deriving instance ToJSON (Tag Create)

-- deriving instance ToJSON (Tag (Front Update))

-- instance Arbitrary (Tag Create) where
--   arbitrary = Tag <$> arbitrary

-- instance Arbitrary (Tag (Front Update)) where
--   arbitrary = Tag <$> arbitrary

-- instance Arbitrary (Tag (Front Display)) where
--   arbitrary = Tag <$> arbitrary

-- instance TestEntity (Tag a)

-- -- instance TestEntity (ID (Tag Create))

-- instance TestEntity (Entity Tag (Front Update))

-- instance TestEntity (Entity Tag (Front Display)) where
--   getFromState = join $ gets getTags

--   withGetEntities tags TestState {..} = TestState {getTags = pure tags, ..}

-- withGetTags :: [Entity Tag (Front Display)] -> StateEndo
-- withGetTags tags TestState{..} = TestState{getTags = pure tags, ..}

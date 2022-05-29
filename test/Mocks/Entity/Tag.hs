{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Mocks.Entity.Tag where

import App.Types
import Control.Monad.State
import Data.Aeson
import Entity.Internal
import Entity.Tag
import HKD.HKD
import Mocks.Arbitrary
import Mocks.TestMonad
import Test.Hspec
import Test.QuickCheck

deriving instance ToJSON (Tag Create)

deriving instance ToJSON (Tag (Front Update))

instance Arbitrary (Tag Create) where
  arbitrary = Tag <$> arbitrary

instance Arbitrary (Tag (Front Update)) where
  arbitrary = Tag <$> arbitrary

instance Arbitrary (Tag (Front Display)) where
  arbitrary = Tag <$> arbitrary

instance TestEntity (Tag a)

instance TestEntity (Entity Tag (Front Update))

instance TestEntity (Entity Tag (Front Display)) where
  getFromState = join $ gets getTags
  withGetEntities tags TestState {..} = TestState {getTags = pure tags, ..}

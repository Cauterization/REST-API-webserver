{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Mocks.Entity.Author where

import App.Types
import Control.Monad.State
import Data.Aeson
import Entity.Author
import Entity.Internal
import HKD.HKD
import Mocks.Arbitrary
import Mocks.Entity.User
import Mocks.TestMonad
import Mocks.Utils
import Test.Hspec
import Test.QuickCheck

deriving instance ToJSON (Author Create)

deriving instance ToJSON (Author (Front Update))

instance Arbitrary (Author Create) where
  arbitrary = Author <$> arbitrary <*> arbitrary

instance Arbitrary (Author (Front Update)) where
  arbitrary = Author <$> arbitrary <*> arbitrary

instance Arbitrary (Author (Front Display)) where
  arbitrary = Author <$> arbitrary <*> arbitrary

instance TestEntity (Author Create)

instance TestEntity (Entity Author (Front Update))

instance TestEntity (Entity Author (Front Display)) where
  getFromState = join $ gets getAuthors
  withGetEntities authors TestState {..} = TestState {getAuthors = pure authors, ..}
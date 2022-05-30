{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Mocks.Entity.Category where

import App.Types
import Control.Monad.State
import Data.Aeson
import Entity.Internal
import Entity.Category
import HKD.HKD
import Mocks.Arbitrary
import Mocks.TestMonad
import Test.Hspec
import Test.QuickCheck

deriving instance (ToJSON (Category (Front Create)))

deriving instance (ToJSON (Category (Front Update)))

instance TestEntity (Category a)

instance Arbitrary (CategoryName a) where
  arbitrary = CategoryName <$> arbitrary

instance Arbitrary (Category (Front Create)) where
  arbitrary = Category <$> arbitrary <*> arbitrary

instance Arbitrary (Category (Front Update)) where
  arbitrary = Category <$> arbitrary <*> arbitrary

instance Arbitrary (Category (Front Display)) where
  arbitrary = Category <$> arbitrary <*> arbitrary

instance TestEntity (Entity Category (Front Update))

instance {-# OVERLAPPING #-} TestEntity (ID (Category (Front Update))) where
  getFromTestDatabase _ = join $ gets getCategoriesID
  withGetEntities catIDs s = s {getCategoriesID = pure catIDs}

instance TestEntity (Entity Category (Front Display)) where
  getFromState = join $ gets getCategories
  withGetEntities cats s = s {getCategories = pure cats}
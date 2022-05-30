{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Mocks.Entity.Picture where

import App.Types
import Control.Monad.State
import Data.Aeson
import Entity.Internal
import Entity.Picture
import HKD.HKD
import Mocks.Arbitrary
import Mocks.TestMonad
import Test.Hspec
import Test.QuickCheck

-- deriving instance (ToJSON (Picture (Front Create)))

-- deriving instance (ToJSON (Category (Front Update)))


instance Arbitrary PictureFormat where
  arbitrary =
    chooseInt (0, 2) >>= \case
      0 -> pure JPEG
      1 -> pure PNG
      2 -> pure GIF
      _ -> undefined

instance Arbitrary (Picture (Front Create)) where
  arbitrary = Picture <$> arbitrary <*> arbitrary

-- instance Arbitrary (Category (Front Update)) where
--   arbitrary = Category <$> arbitrary <*> arbitrary

instance Arbitrary (Picture (Front Display)) where
  arbitrary = Picture <$> arbitrary <*> arbitrary

-- instance TestEntity (Entity Category (Front Update))

-- instance {-# OVERLAPPING #-} TestEntity (ID (Category (Front Update))) where
--   getFromTestDatabase _ = join $ gets getCategoriesID
--   withGetEntities catIDs s = s {getCategoriesID = pure catIDs}

instance TestEntity (Picture Create)

instance TestEntity (Picture (Front Display)) where
  getFromState = join $ gets getPictures
  withGetEntities pics s = s {getPictures = pure pics}

instance TestEntity (Entity Picture (Front Display)) where
  getFromState = join $ gets getEntityPictures
  withGetEntities pics s = s {getEntityPictures = pure pics}
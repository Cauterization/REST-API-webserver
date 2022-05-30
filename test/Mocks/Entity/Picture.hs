module Mocks.Entity.Picture where

import Control.Monad.State (gets, join)
import Entity.Internal (Entity)
import Entity.Picture (Picture (Picture), PictureFormat (..))
import HKD.HKD (Create, Display, Front)
import Mocks.Arbitrary ()
import Mocks.TestMonad
  ( TestEntity (getFromState, withGetEntities),
    TestState (getEntityPictures, getPictures),
  )
import Test.QuickCheck (Arbitrary (arbitrary), chooseInt)

instance Arbitrary PictureFormat where
  arbitrary =
    chooseInt (0, 2) >>= \case
      0 -> pure JPEG
      1 -> pure PNG
      2 -> pure GIF
      _ -> undefined

instance Arbitrary (Picture a) where
  arbitrary = Picture <$> arbitrary <*> arbitrary

instance TestEntity (Picture Create)

instance TestEntity (Picture (Front Display)) where
  getFromState = join $ gets getPictures
  withGetEntities pics s = s {getPictures = pure pics}

instance TestEntity (Entity Picture (Front Display)) where
  getFromState = join $ gets getEntityPictures
  withGetEntities pics s = s {getEntityPictures = pure pics}

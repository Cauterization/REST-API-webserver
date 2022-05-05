module Helpers.Picture where

import Entity.Picture

import Helpers.Internal

import HKD.HKD

import Test.Hspec
import Test.QuickCheck

instance Arbitrary (Picture a) where
    arbitrary = Picture <$> arbitrary <*> arbitrary

instance Arbitrary PictureFormat where
    arbitrary = chooseInt (0, 2) >>= \case
        0 -> pure JPEG
        1 -> pure PNG
        2 -> pure GIF

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Helpers.Picture where

import Entity.Picture ( Picture(Picture), PictureFormat(..) )
import Helpers.Internal ()
import Test.QuickCheck ( chooseInt, Arbitrary(arbitrary) )

instance Arbitrary (Picture a) where
  arbitrary = Picture <$> arbitrary <*> arbitrary

instance Arbitrary PictureFormat where
  arbitrary =
    chooseInt (0, 2) >>= \case
      0 -> pure JPEG
      1 -> pure PNG
      2 -> pure GIF

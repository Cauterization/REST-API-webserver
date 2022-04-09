module Helpers.Internal where

import App.Types

import Data.Aeson

import Extended.Text (Text)
import Extended.Text qualified as T

import HKD.HKD

import Test.QuickCheck
import Control.Monad (replicateM)

instance Arbitrary (ID e) where
    arbitrary = ID <$> chooseInt (1, 100)

instance Arbitrary Text where
    arbitrary = T.pack <$> arbitrary

instance Arbitrary Date where
    arbitrary = toEnum <$> arbitrary

instance {-# OVERLAPPING #-} ToJSON (Maybe NotAllowedFromFront) where
    toJSON _ = Null

instance Arbitrary Value where
    arbitrary = chooseInt (3, 6) >>= \case
        -- 1 -> Object <$> arbitrary
        -- 2 -> Array . fromList <$> do
        --     len <- choose (1, 10)
        --     replicateM len arbitrary
        3 -> String <$> arbitrary
        4 -> Number . realToFrac <$> arbitrary @Double
        5 -> Bool   <$> arbitrary 
        6 -> pure Null 
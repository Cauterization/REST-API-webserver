{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mocks.Arbitrary where

import App.Types (Date, ID (ID))
import Data.ByteString.Lazy qualified as BL
import Entity.Internal (Entity (Entity))
import Extended.Text (Text)
import Extended.Text qualified as T
import Test.QuickCheck (Arbitrary (arbitrary))

instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary BL.ByteString where
  arbitrary = BL.pack <$> arbitrary

deriving newtype instance Arbitrary (ID e)

instance Arbitrary (e a) => Arbitrary (Entity e a) where
  arbitrary = Entity <$> arbitrary <*> arbitrary

instance Arbitrary Date where
  arbitrary = toEnum <$> arbitrary

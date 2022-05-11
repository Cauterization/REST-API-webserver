{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Helpers.Draft where

import Data.Aeson ( ToJSON )
import Data.Text (Text)
import Entity.Article ( Article )
import Entity.Draft ( Draft(Draft) )
import Helpers.Internal()
import HKD.HKD ( Front, Create, Update )
import Test.QuickCheck ( Arbitrary(arbitrary) )

instance Arbitrary (Article a) => Arbitrary (Draft a) where
  arbitrary = Draft <$> arbitrary

deriving instance (Ord (Article a), Eq (Draft a)) => Ord (Draft a)

deriving instance ToJSON (Article (Front Create))

deriving newtype instance ToJSON (Draft (Front Create))

deriving instance ToJSON (Article (Front Update))

deriving newtype instance ToJSON (Draft (Front Update))

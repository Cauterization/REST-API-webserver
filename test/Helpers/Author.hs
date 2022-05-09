module Helpers.Author where

import Data.Aeson
import Data.Coerce

import Entity.Author
import Entity.Internal

import HKD.HKD
import Helpers.Internal
import Helpers.User

import Test.QuickCheck

-- | Post

deriving instance ToJSON (Author (Front Create))
deriving instance ToJSON (Author  Create)

deriving instance Ord      (Author Create)
instance Arbitrary         (Author Create) where
    arbitrary = do
        user        <- arbitrary
        description <- arbitrary
        pure Author{..}

-- | Get

deriving instance Ord      (Author Display)
instance Arbitrary         (Author Display) where
    arbitrary = do
        user        <- arbitrary
        description <- arbitrary
        pure Author{..}

-- | Put

deriving instance Ord    (Author (Front Update))
deriving instance ToJSON (Author (Front Update))
instance Arbitrary       (Author (Front Update)) where
    arbitrary = Author Nothing <$> arbitrary

deriving instance Ord       (Author (Front Display))

module Helpers.Author where

import Data.Aeson

import Entity.Author

import HKD.HKD
import Helpers.Internal
import Helpers.User

import Test.QuickCheck

deriving instance Eq     (Author Create)
deriving instance Ord    (Author Create)
deriving instance ToJSON (Author Create)

instance Arbitrary (Author Create) where
    arbitrary = do
        user        <- arbitrary
        description <- arbitrary
        pure Author{..}

deriving instance Show (Author Display)
deriving instance ToJSON (Author Display)
instance Arbitrary (Author Display) where
    arbitrary = do
        user        <- arbitrary
        description <- arbitrary
        pure Author{..}



deriving instance ToJSON (Author (Front Create))

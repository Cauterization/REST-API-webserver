module Helpers.Author where

import Data.Aeson
import Data.Coerce

import Entity.Author
import Entity.Internal

import HKD.HKD
import Helpers.Internal
import Helpers.User

import Test.QuickCheck

deriving instance Eq       (Author Create)
deriving instance Ord      (Author Create)
deriving instance ToJSON   (Author Create)
deriving instance FromJSON (Author Create)
instance Arbitrary         (Author Create) where
    arbitrary = do
        user        <- arbitrary
        description <- arbitrary
        pure Author{..}

deriving instance Show     (Author Display)
deriving instance Eq       (Author Display)
deriving instance Ord      (Author Display)
deriving instance ToJSON   (Author Display)
deriving instance FromJSON (Author Display)
instance Arbitrary         (Author Display) where
    arbitrary = do
        user        <- arbitrary
        description <- arbitrary
        pure Author{..}

authorDisplayToAuthorFrontDisplay :: Author Display -> Author (Front Display)
authorDisplayToAuthorFrontDisplay a = Author 
    { description = description a
    , user = userDisplayToUserFrontDisplay (entity $ user a)
    }

deriving instance ToJSON (Author (Front Create))

deriving instance Eq       (Author (Front Display))
deriving instance Ord      (Author (Front Display))
deriving instance FromJSON (Author (Front Display))
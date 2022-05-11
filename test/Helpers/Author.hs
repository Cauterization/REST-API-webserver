module Helpers.Author where

import Data.Aeson ( ToJSON )
import Entity.Author ( Author(..) )
import HKD.HKD ( Display, Front, Create, Update )
import Helpers.User ()
import Test.QuickCheck ( Arbitrary(arbitrary) )

-- | Post
deriving instance ToJSON (Author (Front Create))

deriving instance ToJSON (Author Create)

deriving instance Ord (Author Create)

instance Arbitrary (Author Create) where
  arbitrary = do
    user <- arbitrary
    description <- arbitrary
    pure Author {..}

-- | Get
deriving instance Ord (Author Display)

instance Arbitrary (Author Display) where
  arbitrary = do
    user <- arbitrary
    description <- arbitrary
    pure Author {..}

-- | Put
deriving instance Ord (Author (Front Update))

deriving instance ToJSON (Author (Front Update))

instance Arbitrary (Author (Front Update)) where
  arbitrary = Author Nothing <$> arbitrary

deriving instance Ord (Author (Front Display))

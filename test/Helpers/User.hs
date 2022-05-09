module Helpers.User where



import Data.Aeson
import Data.Time qualified as Time

import Entity.User
import Entity.Internal

import HKD.HKD
import Helpers.Internal

import Test.QuickCheck

import Data.Time qualified as Time
import Extended.Text qualified as T

instance ToJSON          (User (Front Create)) where
    toJSON = genericToJSON defaultOptions 
        { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

instance Arbitrary       (User (Front Create)) where
    arbitrary = do
        firstName   <- arbitrary
        lastName    <- arbitrary
        login       <- T.take 4 <$> arbitrary
        let token = Nothing
        password    <- arbitrary
        let registered = Nothing
        admin       <- arbitrary
        pure User{..}

deriving instance Ord      (User Display)


instance Arbitrary         (User Display) where
    arbitrary = do
        firstName   <- arbitrary
        lastName    <- arbitrary
        login       <- T.take 4 <$> arbitrary
        token       <- T.take 4 <$> arbitrary
        password    <- arbitrary
        registered  <- arbitrary
        admin       <- arbitrary
        pure User{..}


deriving instance Ord      (User (Front Display))

deriving instance ToJSON (User  Auth)
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

deriving instance Show   (User (Front Create))
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

-- deriving instance Eq     (User Create)
-- deriving instance Ord    (User Create)

-- userDisplayToUserFrontDisplay :: User Display -> User (Front Display)
-- userDisplayToUserFrontDisplay u = User
--     { firstName = firstName u
--     , lastName  = lastName  u
--     , login     = login     u
--     , token     = Nothing
--     , password  = Nothing
--     , registered   = created   u
--     , admin     = admin     u
--     }

-- instance ToJSON (User Create) where
--     toJSON User{..} = object
--         ["firstName" .= firstName
--         , "lastName" .= lastName
--         , "login"    .= login
--         , "password" .= password
--         , "admin"    .= admin
--         ]

-- instance Arbitrary       (User Create) where
--     arbitrary = do
--         firstName <- arbitrary
--         lastName  <- arbitrary
--         login     <- T.take 4 <$> arbitrary
--         token     <- arbitrary
--         password  <- arbitrary
--         created   <- arbitrary
--         admin     <- arbitrary
--         pure User{..}

-- deriving instance ToJSON (User (Front Create))

deriving instance Eq       (User Display)
deriving instance Ord      (User Display)
-- deriving instance ToJSON   (User Display)
-- instance FromJSON (User Display) where
--     parseJSON = genericParseJSON defaultOptions 
--         { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }


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

-- instance FromJSON (Entity (User ))

deriving instance Eq       (User (Front Display))
deriving instance Ord      (User (Front Display))

-- instance FromJSON (User (Front Display)) where
--     parseJSON = withObject "user front display" $ \o -> do
--         firstName <- o .: "firstName"
--         lastName  <- o .: "lastName"
--         login     <- o .: "login"
--         let token    = Nothing
--         let password = Nothing
--         registered   <- o .: "registered"
--         admin     <- o .: "admin"
--         pure User{..}

-- deriving instance ToJSON (User Auth)

deriving instance Show (User Update)
deriving instance ToJSON (User  Auth)
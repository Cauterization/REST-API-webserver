module Helpers.User where

import Data.Aeson
import Data.Time qualified as Time

import Entity.User

import HKD.HKD
import Helpers.Internal

import Test.QuickCheck

import Data.Time qualified as Time
import qualified Extended.Text as T

deriving instance Eq     (User Create)
deriving instance Ord    (User Create)

instance ToJSON (User Create) where
    toJSON User{..} = object
        ["firstName" .= firstName
        , "lastName" .= lastName
        , "login"    .= login
        , "password" .= password
        , "admin"    .= admin
        ]

instance Arbitrary       (User Create) where
    arbitrary = do
        firstName <- arbitrary
        lastName  <- arbitrary
        login     <- T.take 4 <$> arbitrary
        let token = "super unique token"
        password  <- arbitrary
        let created = Time.fromGregorian 1 2 3
        admin     <- arbitrary
        pure User{..}

deriving instance ToJSON (User (Front Create))
deriving instance Show   (User (Front Create))


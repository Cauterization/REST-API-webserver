{-# LANGUAGE DeriveDataTypeable #-}

module Entity.User where

import Data.Aeson (FromJSON, ToJSON(..), omitNothingFields, defaultOptions, genericToJSON)
import Data.Text (Text)

import GHC.Generics ( Generic )


import HKD.HKD


import Database.PostgreSQL.Simple qualified as Postgres

import Database.Database qualified as Database

import Postgres.Internal

import Data.Data

import Entity.Internal

import App.Types
import App.Internal
import HKD.Utils (Contains, If)
import HKD.EmptyData

data User a = User
  { firstName  :: Field "first_name" 'Required a '[Immutable]                       Text
  , lastName   :: Field "last_name"  'Required a '[Immutable]                       Text
  , login      :: Field "login"      'Required a '[Immutable, Authfield, TU]        Text
  , token      :: Field "token"      'Required a '[NotAllowedFromFront, Hidden, TU] Text
  , password   :: Field "password"   'Required a '[Immutable, Hidden, Authfield]    Text
  , created    :: Field "created"    'Required a '[Immutable, NotAllowedFromFront]  Date
  , admin      :: Field "admin"      'Required a '[Immutable]                       Bool 
  } deriving stock (Generic)

deriving instance Show (User (Front Display))

deriving instance Data             (User Create)
deriving instance Show             (User Create)
deriving instance Data             (User (Front Create))
deriving instance FromJSON         (User (Front Create))
deriving instance Postgres.ToRow   (User Create)

deriving instance Show (User Display)
deriving instance Data             (User Display)
deriving instance Data (Entity User Display)
instance ToJSON (User (Front Display)) where
    toJSON = genericToJSON defaultOptions { omitNothingFields = True }

deriving instance Postgres.FromRow (User  Display)
deriving instance Postgres.FromRow (User  (Front Display))
deriving instance Data             (User (Front Display))
instance Database.GettableFrom Postgres User  (Front Display) where
    getQuery = mconcat
        [ "SELECT firstname, lastname, login, token, password, created, admin"
        , " FROM users" ]

instance Database.GettableFrom Postgres (Entity User) Display where
    getQuery = mconcat
        [ "SELECT id, firstname, lastname, login, token, password, created, admin  "
        , "FROM users" ]

instance Database.PuttableTo Postgres User where

    putQuery = "UPDATE users SET token = ? WHERE id = ?"

deriving instance Data (User Update)

instance Database.ToOneRow (User TokenUpdate) IDs where 

    type instance MkOneRow (User TokenUpdate) IDs 
        = (Maybe Text, ID (Path Current)) 

    toOneRow User{..} [uID] = pure (token, uID)
    toOneRow _ _  = entityIDArityMissmatch "user token update"

deriving instance EmptyData (User TokenUpdate)
deriving instance Data (User TokenUpdate)

deriving instance Postgres.ToRow (User TokenUpdate)
        
data TU

data TokenUpdate
    deriving Data

type instance Field name req TokenUpdate modifiers a =
    If (Contains TU modifiers)
        (Maybe a)
        (Maybe NotUpdated)

deriving instance Data (User Delete)

deriving instance FromJSON (User Auth)

data Authfield

data Auth

type instance Field name req Auth modifiers a = 
    If (Contains Authfield modifiers) 
        (Field name req Create modifiers a)
        (Maybe NotAllowedFromFront)

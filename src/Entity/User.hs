{-# LANGUAGE DeriveDataTypeable #-}

module Entity.User where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics ( Generic )


import HKD.HKD


import Database.PostgreSQL.Simple qualified as Postgres

import Database.Database qualified as Database

import Postgres.Internal

import Data.Data



import App.Types
import App.Internal
import HKD.Utils (Contains, If)
import HKD.EmptyData

data User a = User
  { firstName  :: Field "first_name" 'Required a '[Immutable]                       Text
  , lastName   :: Field "last_name"  'Required a '[Immutable]                       Text
  , login      :: Field "login"      'Required a '[Immutable, Authfield]        Text
  , token      :: Field "token"      'Required a '[NotAllowedFromFront, Hidden] Text
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
deriving instance ToJSON           (User (Front Display))
deriving instance Postgres.FromRow (User  Display)
deriving instance Postgres.FromRow (User  (Front Display))
deriving instance Data             (User (Front Display))
instance Database.GettableFrom Postgres User  (Front Display) where
    getQuery = "SELECT * FROM users"

-- | User token update
data TokenUpdate a = TokenUpdate {tuLogin :: Text, tuToken :: Text}

deriving instance Generic (TokenUpdate Update)
deriving instance Data    (TokenUpdate Update)

instance Database.ToOneRow (TokenUpdate Update) IDs where

    type instance MkOneRow (TokenUpdate Update) IDs 
        = (Text, Text) 

    toOneRow TokenUpdate{..} [] = pure (tuToken, tuLogin)
    toOneRow _ _ = entityIDArityMissmatch "token update"

deriving instance Postgres.ToRow (TokenUpdate Update)

instance Database.PuttableTo Postgres TokenUpdate where

    putQuery = mconcat
        [ "UPDATE users "
        , "SET "
        , "token = ? "
        , "WHERE login = ?"
        ]


deriving instance Data (User Delete)

deriving instance FromJSON (User Auth)

data Authfield

data Auth

type instance Field name req Auth modifiers a = 
    If (Contains Authfield modifiers) 
        (Field name req Create modifiers a)
        (Maybe NotAllowedFromFront)

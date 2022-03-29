{-# LANGUAGE DeriveDataTypeable #-}

module Entities.User where

import Data.Aeson ( ToJSON)
import Data.Text (Text)
import GHC.Generics ( Generic )


import Server.App
import Server.Router

import HKD.Display (Hidden, Display)
import HKD.Field ( Field, Required( Required) )
import HKD.Front ( NotAllowedFromFront, Front )
import HKD.Update (Immutable)

import Database.Database ( Database ) 
import Database.Database qualified as Database
import Database.Postgres (Postgres)
import Database.PostgreSQL.Simple qualified as Postgres

import Data.Data

import Types
import Server.Base


data User a = User
  { firstName  :: Field "first_name" 'Required a '[Immutable]                      Text
  , lastName   :: Field "last_name"  'Required a '[Immutable]                      Text
  , login      :: Field "login"      'Required a '[Immutable]                      Text
  , token      :: Field "token"      'Required a '[NotAllowedFromFront, Hidden]    Text
  , password   :: Field "password"   'Required a '[Immutable          , Hidden]    Text
  , created    :: Field "created"    'Required a '[Immutable, NotAllowedFromFront] Date
  , admin      :: Field "admin"      'Required a '[Immutable, NotAllowedFromFront] Bool 
  } deriving stock (Generic)
    deriving (Database.DBEntity Postgres)

{-}
deriving instance EmptyData        (User Update)
deriving instance Postgres.ToRow   (User Update)

deriving instance FromJSON         (User (Front Create))
deriving instance Postgres.ToRow   (User (Front Create))
-}
deriving instance ToJSON           (User (Front Display))
deriving instance Postgres.FromRow (User (Front Display))
deriving instance Data             (User (Front Display))

instance Routed User Postgres where
    router = do
        --post_   "users"      
        get     "users/me"    getCurrentUser
        --delete_ "admin/users" 
        --post    "login"       loginUser

getCurrentUser :: ( Application m, Database.DBEntity (Database m) User
    ) => Endpoint m
getCurrentUser _ = getE @User []  -- text "getCurrentUser"

loginUser :: Monad m => Endpoint m
loginUser _ = text "login"


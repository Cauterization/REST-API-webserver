{-# LANGUAGE DeriveDataTypeable #-}

module Entity.User where

import Data.Aeson ( ToJSON)
import Data.Text (Text)
import GHC.Generics ( Generic )


import HKD.Display (Hidden, Display)
import HKD.Field ( Field, Required( Required) )
import HKD.Front ( NotAllowedFromFront, Front )
import HKD.Update (Immutable, Update)

import Database.Database ( Database ) 

import Database.PostgreSQL.Simple qualified as Postgres

import Postgres.Internal

import Data.Data

import Data.String

import App.App

data User a = User
  { firstName  :: Field "first_name" 'Required a '[Immutable]                      Text
  , lastName   :: Field "last_name"  'Required a '[Immutable]                      Text
  , login      :: Field "login"      'Required a '[Immutable]                      Text
  , token      :: Field "token"      'Required a '[NotAllowedFromFront, Hidden]    Text
  , password   :: Field "password"   'Required a '[Immutable          , Hidden]    Text
  , created    :: Field "created"    'Required a '[Immutable, NotAllowedFromFront] Date
  , admin      :: Field "admin"      'Required a '[Immutable, NotAllowedFromFront] Bool 
  } deriving stock (Generic)
    -- deriving (Database.GettableSingleFrom Postgres)

{-}
deriving instance EmptyData        (User Update)
deriving instance Postgres.ToRow   (User Update)

deriving instance FromJSON         (User (Front Create))
deriving instance Postgres.ToRow   (User (Front Create))
-}
deriving instance ToJSON           (User (Front Display))
deriving instance Postgres.FromRow (User (Front Display))
deriving instance Data             (User (Front Display))

getCurrentUser :: (Application m, Gettable m User (Front Display)) => Endpoint m
getCurrentUser _ = do
    getEntities @User @(Front Display)[]  -- text "getCurrentUser"

loginUser :: Monad m => Endpoint m
loginUser _ = text "login"

{-

>>> fieldsQuery @(User (Front Display))
Variable not in scope: fieldsQuery
-}


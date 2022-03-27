{-# LANGUAGE DeriveDataTypeable #-}
module Entities.Author where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)

import Database.Postgres(Postgres)
import Database.PostgreSQL.Simple qualified as Postgres
import Database.PostgreSQL.Simple.FromRow qualified as Postgres
import GHC.Generics ( Generic )

import Server.Router


import HKD.Create (Create)
import HKD.Display (Display)
import HKD.EmptyData (EmptyData)
import HKD.Field ( Field, Required(Required) )
import HKD.Filter (Filter)
import HKD.Front ( Front )
import HKD.Update (Update, Immutable)

import Entities.Internal ( EntityOrID )
import Entities.User 

import Database.Database qualified as Database

import Data.Data

data Author a = Author
  { user        :: Field "user"        'Required a '[Immutable]        (EntityOrID User a)
  , description :: Field "description" 'Required a '[]                                Text
  } deriving stock (Generic)
    deriving (Database.Entity Postgres)

deriving instance EmptyData (Author Update)
deriving instance FromJSON  (Author Update)

deriving instance EmptyData (Author Filter)

deriving instance FromJSON  (Author (Front Create))

deriving instance ToJSON    (Author Display)
deriving instance Data      (Author Display)
instance Postgres.FromRow   (Author Display) where
    fromRow =  do
        user        <- Postgres.fromRow
        description <- Postgres.field
        pure Author{..}

instance Routed Author Postgres where
    router = do
        --post_   "admin/authors"         
        get_    "admin/authors"          
        --get_    "admin/authors/{ID}" 
        --put_    "admin/authors/{ID}"     
        --delete_ "admin/authors/{ID}"  
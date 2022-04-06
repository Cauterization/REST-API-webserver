{-# LANGUAGE DeriveDataTypeable #-}
module Entity.Author where

import Control.Monad.Identity

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)

import Postgres.Internal(Postgres)
import Database.PostgreSQL.Simple qualified as Postgres
import Database.PostgreSQL.Simple.FromRow qualified as Postgres

import GHC.Generics ( Generic )


import App.Internal


import Data.Coerce
import HKD.HKD

import Entity.Internal ( EntityOrID, NamedID, fieldsQuery )
import Entity.User 

import Database.Database qualified as Database

import Data.Data
import Data.String

data Author a = Author
  { user        :: Field "user"        'Required a '[Immutable] (EntityOrID User a)
  , description :: Field "description" 'Required a '[]          Text
  } deriving stock (Generic)

deriving instance EmptyData (Author Update)
deriving instance FromJSON  (Author Update)

deriving instance EmptyData (Author Filter)

deriving instance FromJSON  (Author (Front Create))

deriving instance ToJSON    (Author (Front Display))
deriving instance Data      (Author (Front Display))
instance Postgres.FromRow   (Author (Front Display)) where
    fromRow =  do
        user        <- Postgres.fromRow
        description <- Postgres.field
        pure Author{..}

instance Database.GettableFrom Postgres Author (Front Display) where

    getQuery = Database.qSELECT (fieldsQuery @(User (Front Display)) <> ", description") 
         <> Database.qFROM "authors_view"

-- instance Database.GettableSingleFrom Postgres Author where
--     getEByIDQuery = 
--         
  
-- instance Database.GettableManyFrom Postgres Author where
--     getEQuery = 
--         Database.qSELECT (fieldsQuery @(User (Front Display)) <> ", description") 
--         <> Database.qFROM "authors_view"
-- {- 


{-
>>> Database.unQuery $ Database.getQuery @Postgres @Database.Many @Author @(Front Display) 0
"SELECT description, firstName, lastName, login, token, password, created, admin FROM Users_view  OFFSET 0 * (? - 1)"
>>> Database.qSELECT "description" <> coerce (Database.getQuery @Postgres @Identity @User @(Front Display) 0)
Query {eqSELECT = Just "description, firstName, lastName, login, token, password, created, admin", eqFROM = Just "Users_view ", eqWHERE = Nothing, eqLIMIT = Nothing, eqOFFSET = Nothing}
-}

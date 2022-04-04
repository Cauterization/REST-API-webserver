{-# LANGUAGE DeriveDataTypeable #-}
module Entities.Author where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)

import Postgres.Postgres(Postgres)
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

import Entities.Internal ( EntityOrID, NamedID, fieldsQuery )
import Entities.User 

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

instance Database.DBEntity Postgres Author where

    type DBArity Author = Database.Unary

instance Database.GettableSingleFrom Postgres Author where
    getEByIDQuery = 
        Database.qSELECT (fieldsQuery @(User (Front Display)) <> ", description") 
        <> Database.qFROM "authors_view"
  
instance Database.GettableManyFrom Postgres Author where
    getEQuery = 
        Database.qSELECT (fieldsQuery @(User (Front Display)) <> ", description") 
        <> Database.qFROM "authors_view"
{- 
>>> fieldsQuery @(User (Front Display))
parse error (possibly incorrect indentation or mismatched brackets)
-}

instance Routed Author Postgres where
    router = do
        --post_   "admin/authors"         
        get_    "admin/authors"          
        --get_    "admin/authors/{ID}" 
        --put_    "admin/authors/{ID}"     
        --delete_ "admin/authors/{ID}"  


{-

>>> Database.getEQueryDefault @Postgres @Author @(Front Display)
EQuery {eqSELECT = Just "", eqFROM = Just "Authors_view ", eqWHERE = Nothing, eqLIMIT = Nothing, eqOFFSET = Nothing}



>>>  fromString $ tail $ init $ show ("SELECT 1 " <> fieldsQuery @(User (Front Display)) :: Postgres.Query) :: Database.EQuery Postgres (User (Front Display))
EQuery {eqSELECT = Just "1", eqFROM = Just "", eqWHERE = Just "", eqLIMIT = Just "", eqOFFSET = Just ""}


>>> fromString "SELECT 1 "  :: Database.EQuery Postgres (User (Front Display))
EQuery {eqSELECT = Just "1", eqFROM = Just "", eqWHERE = Just "", eqLIMIT = Just "", eqOFFSET = Just ""}





-}

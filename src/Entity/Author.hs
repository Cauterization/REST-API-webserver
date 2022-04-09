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

import App.Types


data Author a = Author
  { user        :: Field "user"        'Required a '[Immutable] (EntityOrID User a)
  , description :: Field "description" 'Required a '[]          Text
  } deriving stock (Generic)



deriving instance EmptyData (Author Filter)

deriving instance FromJSON  (Author (Front Create))
deriving instance Postgres.ToRow (Author Create)
deriving instance Data (Author Create)
deriving instance Show (Author Create)
deriving instance Show (Author (Front Create))
instance Database.PostableTo Postgres Author where

    postQuery = " INSERT INTO authors (user_id, description) VALUES (?,?)"

deriving instance Show      (Author (Front Display))
deriving instance ToJSON    (Author (Front Display))
deriving instance Data      (Author (Front Display))
instance Postgres.FromRow   (Author (Front Display)) where
    fromRow =  do
        user        <- Postgres.fromRow
        description <- Postgres.field
        pure Author{..}
instance Database.GettableFrom Postgres Author (Front Display) where

    getQuery = "SELECT *  FROM authors_view"

deriving instance Data (Author (Front Update))
deriving instance Data (Author Update)
deriving instance EmptyData (Author Update)
deriving instance FromJSON  (Author (Front Update))
deriving instance Postgres.ToRow (Author Update)
instance Database.ToOneRow (Author (Front Update)) IDs where

    type instance MkOneRow (Author (Front Update)) IDs 
        = (Maybe NotUpdated, Maybe Text, ID (Path Current)) 

    toOneRow Author{..} [aID] = pure (user, description, aID)
    toOneRow _ _ = entityIDArityMissmatch "post author"

instance Database.PuttableTo Postgres Author where

    putQuery = mconcat
        [ "UPDATE Authors "
        , "SET "
        , "user_id     = COALESCE (?, user_id), "
        , "description = COALESCE (?, description) "
        , "WHERE id = ?"
        ]

-- class ToOneRow a b where

--     type family MkOneRow a b :: Type 

--     toOneRow :: a -> b -> MkOneRow a b 


deriving instance Data (Author Delete)


-- deriving instance Postgres.ToRow (Author Delete)
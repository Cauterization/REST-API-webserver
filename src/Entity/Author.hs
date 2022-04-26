{-# LANGUAGE DeriveDataTypeable #-}
module Entity.Author where


import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)

import Postgres.Internal(Postgres)
import Database.PostgreSQL.Simple qualified as Postgres
import Database.PostgreSQL.Simple.FromRow qualified as Postgres

import Data.Generics.Product.Fields qualified as GL
import GHC.Generics ( Generic )

import App.Internal


import HKD.HKD

import Entity.Internal ( EntityOrID, Entity )
import Entity.User 

import Database.Database qualified as Database

import Data.Data

import App.Types

data Author a = Author
  { user        :: Field 'Required a '[Immutable] (EntityOrID User a)
  , description :: Field 'Required a '[]          Text
  } deriving stock (Generic)

instance
  {-# OVERLAPPING #-}
  (GL.HasField' name (Author f) a, f ~ g, a ~ b) =>
  GL.HasField name (Author f) (Author g) a b
  where
  field = GL.field' @name

-- | Post / Create

deriving instance FromJSON       (Author (Front Create))
deriving instance Show           (Author Create)
deriving instance Data           (Author Create)
deriving instance Postgres.ToRow (Author Create)
instance Database.Postable        Author Create where
    postQuery = " INSERT INTO authors (user_id, description) VALUES (?,?)"

-- | Get / Front Display

deriving instance Show    (Author (Front Display))
deriving instance Data    (Author (Front Display))
deriving instance ToJSON  (Author (Front Display))
instance Postgres.FromRow (Author (Front Display)) where
    fromRow =  do
        user        <- Postgres.fromRow
        description <- Postgres.field
        pure Author{..}

instance Database.Gettable (Entity Author) (Front Display) where            
    getQuery = mconcat
        [ "SELECT "
        , "id, "
        , "user_id, "
        , fieldsQuery @(User (Front Display))
        , ", description"
        , " FROM authors_view"
        ]

-- | Put

deriving instance FromJSON (Author (Front Update))
deriving instance Data (Author (Front Update))
deriving instance Postgres.ToRow (Author (Front Update))
instance Database.Puttable (Entity Author) (Front Update) where

    putQuery = mconcat
        [ "UPDATE authors "
        , "SET user_id = COALESCE (?, user_id), "
        , "description = COALESCE (?, description) "
        , "WHERE id = ? "
        ]

-- | Delete

deriving instance Data             (Author  Delete)

-- deriving instance FromJSON  (Author (Front Create))
-- deriving instance Postgres.ToRow (Author Create)
-- deriving instance Data (Author Create)
-- deriving instance Show (Author Create)
-- deriving instance Show (Author (Front Create))
-- instance Database.PostableTo Postgres Author where

--    

-- deriving instance Show      (Author (Front Display))
-- deriving instance ToJSON    (Author (Front Display))
-- deriving instance Data      (Author (Front Display))
-- instance Postgres.FromRow   (Author (Front Display)) where
--     fromRow =  do
--         user        <- Postgres.fromRow
--         description <- Postgres.field
--         pure Author{..}
-- instance Database.GettableFrom Postgres Author (Front Display) where

--     getQuery = mconcat
--         [ "SELECT ", fieldsQuery @(User (Front Display))
--         , ", description"
--         , " FROM authors_view"
--         ]


-- deriving instance Data           (Author Update)
-- deriving instance EmptyData      (Author Update)
-- deriving instance Postgres.ToRow (Author Update)

-- deriving instance Data     (Author (Front Update))
-- deriving instance FromJSON (Author (Front Update))
-- instance Database.ToOneRow (Author (Front Update)) IDs where

--     type instance MkOneRow (Author (Front Update)) IDs 
--         = (Maybe NotUpdated, Maybe Text, ID (Path Current)) 

--     toOneRow Author{..} [aID] = pure (user, description, aID)
--     toOneRow _ _ = entityIDArityMissmatch "update author"

-- instance Database.PuttableTo Postgres Author Update where

--     putQuery = mconcat
--         [ "UPDATE Authors "
--         , "SET "
--         , "user_id     = COALESCE (?, user_id), "
--         , "description = COALESCE (?, description) "
--         , "WHERE id = ?"
--         ]

-- deriving instance Data (Author Delete)

-- deriving instance Data (Author Display)
-- instance Postgres.FromRow (Author Display) where
--     fromRow = do
--         user        <- Postgres.fromRow
--         description <- Postgres.field
--         pure Author{..}

-- instance Database.GettableFrom Postgres (Entity Author) Display where

--     getQuery = mconcat
--         [ "SELECT id, user_id, firstname, lastname, login, token, "
--         , "password, registered, admin, "
--         , "description "
--         , "FROM authors_view"
--         ]

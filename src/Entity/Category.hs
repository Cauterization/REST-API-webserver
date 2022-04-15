{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Entity.Category where

import App.Types
import App.Internal

import Data.Aeson
import Data.Coerce
import Data.Data
import Data.List
import Data.String

import Database.Database qualified as Database

import Database.PostgreSQL.Simple           qualified as Postgres
import Database.PostgreSQL.Simple.ToRow     qualified as Postgres
import Database.PostgreSQL.Simple.ToField   qualified as Postgres
import Database.PostgreSQL.Simple.FromField qualified as Postgres
import Database.PostgreSQL.Simple.FromRow   qualified as Postgres
import Database.PostgreSQL.Simple.Types     qualified as Postgres

import Entity.Internal

import Extended.Text (Text)
import Extended.Text qualified as T

import GHC.Generics

import HKD.HKD

import Postgres.Internal

data Category a = Category
  { name ::   Field "name"   'Required a '[] (CategoryName a)
  , parent :: Field "parent" 'Required a '[] (CatParent a)
  } deriving stock Generic

newtype CategoryName a = CategoryName {unCatName :: Text}
    deriving newtype (Show, FromJSON, ToJSON, Eq, Postgres.ToField, Postgres.FromField)
    deriving stock Data

type family CatParent a where
    CatParent Create          = Maybe (ID (Category Create))
    CatParent (Front Display) = [CategoryName Display]
    CatParent (Front Update)  = (ID (Category (Front Update)))
    CatParent Delete          = ()
    CatParent a               = Maybe (ID (Category Display)) -- for tests

deriving instance Data           (Category Create)
deriving instance Show           (Category Create)
deriving instance FromJSON       (Category Create) 
deriving instance Postgres.ToRow (Category Create)
instance Database.PostableTo Postgres Category where
    tableNamePost = "categories"

deriving instance Data    (Category (Front Display))
deriving instance Show    (Category (Front Display))
instance ToJSON           (Category (Front Display)) where
    toJSON Category{..} = toJSON $ map unCatName $ coerce name : parent
instance Postgres.FromRow (Category (Front Display)) where
    fromRow = do
        name   <- Postgres.field
        parent <- Postgres.fromPGArray <$> Postgres.field
        pure Category{..}
instance Database.GettableFrom Postgres Category (Front Display) where
    getQuery = "SELECT last, branch[2:] FROM cat_branches"


deriving instance Data (Category (Front Update))
deriving instance FromJSON (Category (Front Update))
instance  Database.PuttableTo Postgres Category where 

    putQuery = mconcat
        [ "UPDATE categories "
        , "SET name = COALESCE (?, name), "
        , " parent = COALESCE (?, parent) "
        , "WHERE id = ? "
        ]

type CatPutFiels = 
    ( Maybe (CategoryName (Front Update))
    , Maybe (ID (Category (Front Update)))
    , ID (Path Current)) 


instance Database.ToOneRow  (Category (Front Update)) IDs where

    type instance MkOneRow (Category (Front Update)) IDs 
        = CatPutFiels
    toOneRow Category{..} [cID] = pure (name, parent, cID)
    toOneRow _ _ = entityIDArityMissmatch "update tag"

-- | Cycles checking on category update
instance Database.GettableFrom Postgres ID (Category (Front Update)) where

    getQuery = mconcat
        [ "SELECT unnest(parents) "
        , "FROM cat_parents "
        , "WHERE id = ? "
        ]

deriving instance Data (Category Delete)

instance Database.DeletableFrom Postgres Category where

    deleteQuery = mconcat
        [ "WITH ARG (cat_id) AS (VALUES (?)), "
        , "UPD AS "
        ,   "(UPDATE categories "
        ,   "SET parent = (SELECT parent FROM categories WHERE id = "
        ,       "(SELECT cat_id FROM ARG)) "
        ,   "WHERE parent = (SELECT cat_id FROM ARG) "
        ,   "RETURNING (SELECT cat_id FROM ARG)"
        , ") "
        , "DELETE FROM categories "
        , "WHERE id =  "
        , "(SELECT cat_id FROM UPD UNION ALL SELECT cat_id FROM ARG "
        , "LIMIT 1 )"
        ]
    -- deleteQuery = mconcat
    --     [ "WITH ARG (cat_id) AS (VALUES (?)), "
    --     , "UPD AS "
    --     ,   "(UPDATE categories "
    --     ,   "SET parent = (SELECT parent FROM categories WHERE id = "
    --     ,       "(SELECT cat_id FROM ARG)) "
    --     ,   "WHERE parent = (SELECT cat_id FROM ARG) "
    --     ,   "RETURNING null"
    --     , ")"
    --     , "DELETE FROM categories "
    --     , "WHERE id = (SELECT cat_id FROM UPD CROSS JOIN ARG)"
    --     ]


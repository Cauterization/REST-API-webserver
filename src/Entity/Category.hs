{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Entity.Category where

import App.Types
import Data.Aeson
import Data.Coerce
import Data.Data
import Data.String
import Database.Database qualified as Database
import Database.PostgreSQL.Simple qualified as Postgres
import Database.PostgreSQL.Simple.FromField qualified as Postgres
import Database.PostgreSQL.Simple.FromRow qualified as Postgres
import Database.PostgreSQL.Simple.ToField qualified as Postgres
import Database.PostgreSQL.Simple.Types qualified as Postgres
import Entity.Internal
import Extended.Text (Text)
import GHC.Generics
import HKD.HKD

data Category a = Category
  { name :: Field a '[] (CategoryName a),
    parent :: Field a '[] (CatParent a)
  }
  deriving stock (Generic)

newtype CategoryName a = CategoryName {unCatName :: Text}
  deriving newtype (Show, IsString, FromJSON, ToJSON, Eq, Ord, Postgres.ToField, Postgres.FromField)
  deriving stock (Data)

type family CatParent a where
  CatParent Create = Maybe (ID (Category Create))
  CatParent (Front Display) = [CategoryName Display]
  CatParent (Front Update) = (ID (Category (Front Update)))
  CatParent Delete = ()
  CatParent a = Maybe (ID (Category Display)) -- for tests

deriving instance
  ( Data a,
    Data (Field a '[] (CategoryName a)),
    Data (Field a '[] (CatParent a))
  ) =>
  Data (Category a)

deriving instance
  ( Show (Field a '[] (CategoryName a)),
    Show (Field a '[] (CatParent a))
  ) =>
  Show (Category a)

deriving instance
  ( Eq (Field a '[] (CategoryName a)),
    Eq (Field a '[] (CatParent a))
  ) =>
  Eq (Category a)

-- | Post / Create
deriving instance FromJSON (Category (Front Create))

deriving instance Postgres.ToRow (Category Create)

-- | Get / Front Display
instance ToJSON (Category (Front Display)) where
  toJSON Category {..} = toJSON $ map unCatName $ coerce name : parent

instance Postgres.FromRow (Category (Front Display)) where
  fromRow = do
    name <- Postgres.field
    parent <- Postgres.fromPGArray <$> Postgres.field
    pure Category {..}

instance Database.Gettable (Entity Category) (Front Display) where
  getQuery = "SELECT id, last, branch[2:] FROM cat_branches"

-- | Put
deriving instance FromJSON (Category (Front Update))

deriving instance Postgres.ToRow (Category (Front Update))

-- | Cycles checking on category update
instance Database.Gettable ID (Category (Front Update)) where
  getQuery =
    mconcat
      [ "SELECT unnest(parents) ",
        "FROM cat_parents ",
        "WHERE id = ? "
      ]

--
--

-- instance Database.PostableTo Postgres Category where
--     tableNamePost = "categories"

-- deriving instance Data    (Category (Front Display))
-- deriving instance Show    (Category (Front Display))

-- instance Postgres.FromRow (Category (Front Display)) where
--     fromRow = do
--
-- instance Database.GettableFrom Postgres Category (Front Display) where
--     getQuery = "SELECT last, branch[2:] FROM cat_branches"

-- deriving instance Data (Category (Front Update))
-- deriving instance FromJSON (Category (Front Update))
-- instance Database.PuttableTo Postgres Category (Front Update) where

--     putQuery = mconcat
--         [ "UPDATE categories "
--         , "SET name = COALESCE (?, name), "
--         , " parent = COALESCE (?, parent) "
--         , "WHERE id = ? "
--         ]

-- type CatPutfields =
--     ( Maybe (CategoryName (Front Update))
--     , Maybe (ID (Category (Front Update)))
--     , ID (Path Current))

-- instance Database.ToOneRow  (Category (Front Update)) IDs where

--     type instance MkOneRow (Category (Front Update)) IDs
--         = CatPutfields
--     toOneRow Category{..} [cID] = pure (name, parent, cID)
--     toOneRow _ _ = entityIDArityMissmatch "update tag"

-- deriving instance Data (Category Delete)

-- instance Database.DeletableFrom Postgres Category where

--     deleteQuery = mconcat
--         [ "WITH ARG (cat_id) AS (VALUES (?)), "
--         , "UPD AS "
--         ,   "(UPDATE categories "
--         ,   "SET parent = (SELECT parent FROM categories WHERE id = "
--         ,       "(SELECT cat_id FROM ARG)) "
--         ,   "WHERE parent = (SELECT cat_id FROM ARG) "
--         ,   "RETURNING (SELECT cat_id FROM ARG)"
--         , ") "
--         , "DELETE FROM categories "
--         , "WHERE id =  "
--         , "(SELECT cat_id FROM UPD UNION ALL SELECT cat_id FROM ARG "
--         , "LIMIT 1 )"
--         ]
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

-- deriving instance Data (Category Display)
-- deriving instance Data (Category Update)

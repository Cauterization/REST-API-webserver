{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Entity.Category where

import App.Types ( ID )
import Data.Aeson ( FromJSON, ToJSON(toJSON) )
import Data.Coerce ( coerce )
import Data.Data ( Data )
import Data.String ( IsString )
import Database.Database qualified as Database
import Database.PostgreSQL.Simple qualified as Postgres
import Database.PostgreSQL.Simple.FromField qualified as Postgres
import Database.PostgreSQL.Simple.FromRow qualified as Postgres
import Database.PostgreSQL.Simple.ToField qualified as Postgres
import Database.PostgreSQL.Simple.Types qualified as Postgres
import Entity.Internal ( Entity )
import Extended.Text (Text)
import GHC.Generics ( Generic )
import HKD.HKD ( Field, Create, Delete, Display, Update, Front )

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
{-# LANGUAGE ImportQualifiedPost #-}

module Entity.Tag where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.Text (Text)
import Database.Delete qualified as Database
import Database.Get qualified as Database
import Database.Post qualified as Database
import Database.Put qualified as Database
import Entity.Internal (Entity(..))
import Extended.Postgres qualified as Postgres
import GHC.Generics (Generic)
import HKD.HKD (Create, Display, Field, Front, Update)

newtype Tag a = Tag
  { tag :: Field a '[] Text
  }
  deriving stock (Generic)

deriving instance
  ( Data a,
    Data (Field a '[] Text)
  ) =>
  Data (Tag a)

deriving instance
  ( Show (Field a '[] Text)
  ) =>
  Show (Tag a)

deriving instance
  ( Eq (Field a '[] Text)
  ) =>
  Eq (Tag a)

-- | Post / Create
deriving anyclass instance FromJSON (Tag (Front Create))

deriving anyclass instance Postgres.ToRow (Tag Create)

instance Database.Postable Tag

-- | Get / Front Display
deriving anyclass instance ToJSON (Tag (Front Display))

deriving anyclass instance Postgres.FromRow (Tag (Front Display))

instance Database.Gettable (Entity Tag) (Front Display)

-- | Update
deriving anyclass instance FromJSON (Tag (Front Update))

deriving anyclass instance Postgres.ToRow (Tag (Front Update))

instance Postgres.ToRow (Entity Tag (Front Update)) where
  toRow Entity {..} = Postgres.toRow entity ++ Postgres.toRow entityID

instance Database.Puttable (Tag (Front Update))

-- | Delete
instance Database.Deletable Tag

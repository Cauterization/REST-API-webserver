{-# LANGUAGE DeriveDataTypeable #-}

module Entity.Tag where

import Data.Aeson ( FromJSON, ToJSON )
import Data.Data ( Data )
import Data.Text (Text)
import Extended.Postgres qualified as Postgres
import GHC.Generics (Generic)
import HKD.HKD ( Field, Create, Display, Update, Front )

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

-- | Get / Front Display
deriving anyclass instance ToJSON (Tag (Front Display))

deriving anyclass instance Postgres.FromRow (Tag (Front Display))

-- | Update
deriving anyclass instance FromJSON (Tag (Front Update))

deriving anyclass instance Postgres.ToRow (Tag (Front Update))

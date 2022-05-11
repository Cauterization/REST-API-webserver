{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Entity.Tag where

import Data.Aeson
import Data.Data
import Data.Text (Text)
import Extended.Postgres qualified as Postgres
import GHC.Generics (Generic)
import HKD.HKD

newtype Tag a = Tag
  { tag :: Field a '[] Text
  }
  deriving stock (Generic)

deriving instance
  ( Data a,
    Data (Field a '[] Text)
  ) =>
  Data (Tag a)

-- | Post / Create
deriving anyclass instance FromJSON (Tag (Front Create))

deriving instance Show (Tag Create)

deriving anyclass instance Postgres.ToRow (Tag Create)

-- | Get / Front Display
deriving instance Eq (Tag (Front Display))

deriving instance Show (Tag (Front Display))

deriving anyclass instance ToJSON (Tag (Front Display))

deriving anyclass instance Postgres.FromRow (Tag (Front Display))

-- | Update
deriving anyclass instance FromJSON (Tag (Front Update))

deriving anyclass instance Postgres.ToRow (Tag (Front Update))

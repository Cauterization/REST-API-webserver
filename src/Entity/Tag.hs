{-# LANGUAGE DeriveDataTypeable #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Entity.Tag where

import App.Internal

import Data.Aeson
import Data.Data 
import Data.Text (Text)

import Entity.Internal

import Database.Database qualified as Database
import Extended.Postgres qualified as Postgres

import Postgres.Internal

import GHC.Generics (Generic)
import HKD.HKD
import App.Types (IDs, Current, ID, Path)

newtype Tag a = Tag
  { tag :: Field  a '[] Text
  } deriving stock Generic

deriving instance 
    ( Data a
    , Data (Field  a '[] Text)
    ) => Data (Tag a)

-- | Post / Create

deriving anyclass instance FromJSON         (Tag (Front Create))
deriving instance Show                      (Tag Create)
deriving anyclass instance Postgres.ToRow   (Tag Create)

-- | Get / Front Display

deriving instance Eq                        (Tag (Front Display))
deriving instance Show                      (Tag (Front Display))
deriving anyclass instance ToJSON           (Tag (Front Display))
deriving anyclass instance Postgres.FromRow (Tag (Front Display))

-- | Update

deriving anyclass instance FromJSON         (Tag (Front Update))
deriving anyclass instance Postgres.ToRow   (Tag (Front Update))

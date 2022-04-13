{-# LANGUAGE DeriveDataTypeable #-}
module Entity.Tag where

import App.Internal

import Data.Aeson
import Data.Data 
import Data.Text (Text)

import Database.Database qualified as Database
import Database.PostgreSQL.Simple qualified as Postgres

import Postgres.Internal

import GHC.Generics (Generic)
import HKD.HKD
import App.Types (IDs, Current, ID, Path)

newtype Tag a = Tag
  { tag :: Field "name" 'Required a '[] Text
  } deriving stock Generic

deriving instance Show           (Tag Create)
deriving instance Data           (Tag Create)
deriving instance FromJSON       (Tag Create)
deriving instance Postgres.ToRow (Tag Create)
deriving instance Database.PostableTo Postgres Tag

deriving instance Data     (Tag Update)
deriving instance FromJSON (Tag Update)

deriving instance Data     (Tag (Front Update))
deriving instance FromJSON (Tag (Front Update))

deriving instance Show                           (Tag (Front Display))
deriving instance Data                           (Tag (Front Display))
deriving instance ToJSON                         (Tag (Front Display))
deriving instance Postgres.FromRow               (Tag (Front Display))
deriving instance Database.GettableFrom Postgres  Tag (Front Display)

deriving instance Postgres.ToRow (Tag (Front Update))
instance Database.ToOneRow       (Tag (Front Update)) IDs where

    type instance MkOneRow (Tag (Front Update)) IDs 
        = (Maybe Text, ID (Path Current)) 

    toOneRow Tag{..} [aID] = pure (tag, aID)
    toOneRow _ _ = entityIDArityMissmatch "update tag"


deriving instance Data (Tag Delete)







{-# LANGUAGE DeriveDataTypeable #-}
module Entity.Internal where

import Data.Aeson
import Data.Data
import Data.String
import Data.Maybe
import Data.Kind (Type)
import Data.List

import Database.PostgreSQL.Simple qualified as Postgres
import Database.PostgreSQL.Simple.FromField qualified as Postgres
import Database.PostgreSQL.Simple.FromRow qualified as Postgres

import Postgres.Internal qualified as Postgres
import GHC.Generics (Generic)

import App.Types

import HKD.HKD

data Entity e a = Entity
  { entityID :: NamedID "_id" a e
  , entity :: e a }
  deriving stock Generic

deriving instance (Data (e a) , Data (NamedID "_id" a e), Typeable e, Typeable a)
    => Data (Entity e a)
deriving instance (FromJSON (e a), FromJSON (NamedID "_id" a e))
    => FromJSON (Entity e a)
deriving instance (Show (e a), Show (NamedID "_id" a e))
    => Show (Entity e a)
deriving instance (ToJSON (e a), ToJSON (NamedID "_id" a e))
    => ToJSON (Entity e a)
instance (Postgres.FromRow (e a), Postgres.FromField (NamedID "_id" a e))
    => Postgres.FromRow (Entity e a) where
        fromRow = do
            entityID <- Postgres.field
            entity   <- Postgres.fromRow
            pure Entity{..}

type family NamedID n a (e :: * -> *) :: * where
    NamedID n Schema          e = Named n
    NamedID n Filter          e =   [ID (e Filter)]
    NamedID n a               e =    ID (e a)

type family EntityOrID (e :: Type -> Type) a :: * where
    EntityOrID e Create          = ID (e Create)
    EntityOrID e (Front Create)  = ID (e (Front Create))
    EntityOrID e Update          = NamedID "_id" Update e
    EntityOrID e (Front Display) = e (Front Display)
    EntityOrID e Display         = Entity e Display
    EntityOrID e Delete          = ID (e Delete)
    EntityOrID e Publish         = ID (e Publish)


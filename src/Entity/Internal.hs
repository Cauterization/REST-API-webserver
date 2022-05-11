{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Entity.Internal where

import App.Types
import Data.Aeson
import Data.Data
import Data.Generics.Product.Fields qualified as GL
import Data.HashMap.Strict qualified as HMAP
import Data.Kind (Type)
import Extended.Postgres qualified as Postgres
import GHC.Generics (Generic)
import HKD.HKD

data Entity e a = Entity
  { entityID :: ID (e a),
    entity :: e a
  }
  deriving stock (Generic)

deriving instance
  (Show (e a)) =>
  Show (Entity e a)

deriving instance
  Eq (e a) =>
  Eq (Entity e a)

deriving instance
  (Data (e a), Typeable e, Typeable a) =>
  Data (Entity e a)

instance
  {-# OVERLAPPABLE #-}
  (GL.HasField' name (Entity e f) a, f ~ g, a ~ b) =>
  GL.HasField name (Entity e f) (Entity e g) a b
  where
  field = GL.field' @name

instance {-# OVERLAPPABLE #-} (FromJSON (e a), Typeable e) => FromJSON (Entity e a) where
  parseJSON = withObject ("Entity " <> nameOf @e) $ \o -> do
    entityID <- o .: "id"
    entity <- o .: (nameOf @e)
    pure Entity {..}

instance {-# OVERLAPPABLE #-} (ToJSON (e a), Typeable e) => ToJSON (Entity e a) where
  toJSON Entity {..} = case toJSON entity of
    Object o -> Object $ HMAP.insert "id" (toJSON entityID) o
    x -> object ["id" .= entityID, nameOf @e .= x]

instance
  (Postgres.FromRow (e a)) =>
  Postgres.FromRow (Entity e a)
  where
  fromRow = do
    entityID <- Postgres.field
    entity <- Postgres.fromRow
    pure Entity {..}

instance {-# OVERLAPPABLE #-} Postgres.ToRow (e a) => Postgres.ToRow (Entity e a) where
  toRow Entity {..} = Postgres.toRow entity ++ Postgres.toRow entityID

type family EntityOrID (e :: Type -> Type) a :: * where
  EntityOrID e (Front Display) = Entity e (Front Display)
  EntityOrID e Display = Entity e Display
  EntityOrID e a = ID (e a)

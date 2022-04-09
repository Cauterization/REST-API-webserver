module Entity.Internal where

import Data.Aeson
import Data.Data
import Data.String
import Data.Maybe
import Data.Kind (Type)
import Data.List

import GHC.Generics (Generic)

import App.Types

import HKD.HKD

data Entity e a = Entity
  { entityID :: NamedID "_id" a e
  , entity :: e a }
  deriving stock Generic

deriving instance (FromJSON (e a), FromJSON (NamedID "_id" a e))
    => FromJSON (Entity e a)
deriving instance (ToJSON (e a), ToJSON (NamedID "_id" a e))
    => ToJSON (Entity e a)

type family NamedID n a (e :: * -> *) :: * where
    NamedID n Schema          e = Named n
    NamedID n Filter          e =   [ID (e Filter)]
    NamedID n (Front Display) e = e (Front Display)
    NamedID n a               e =    ID (e a)

type family EntityOrID (e :: Type -> Type) a :: * where
    EntityOrID e Create          = ID (e Create)
    EntityOrID e (Front Create)  = ID (e (Front Create))
    EntityOrID e Update          = NamedID "_id" Update e
    EntityOrID e (Front Display) = e (Front Display)
    EntityOrID e Display         = Entity e Display
    EntityOrID e Delete          = ID (e Delete)

nameOf :: forall (e :: Type -> Type) s. (Typeable e, IsString s) => s
nameOf = let t = show (typeOf (Proxy @e))
         in fromString $ fromMaybe t $ stripPrefix "Proxy (* -> *) " t

fieldsOf :: forall e. Data e => [String]
fieldsOf = concatMap constrFields . dataTypeConstrs . dataTypeOf $ (undefined :: e)

fieldsQuery :: forall e s. (Data e, IsString s) => s
fieldsQuery = fromString $ intercalate ", " $ fieldsOf @e

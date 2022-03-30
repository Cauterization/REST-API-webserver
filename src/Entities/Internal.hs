{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Entities.Internal where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Types qualified as J
import Data.Data
import Data.List (stripPrefix, intercalate)
import Data.Maybe
import Data.String

import GHC.Generics ( Generic )

import HKD.Create ( Create )
import HKD.Display ( Display )
import HKD.Filter ( Filter )
import HKD.Front ( Front )
import HKD.Schema ( Named, Schema) 
import HKD.Update ( Update )
import Types

data Entity e a = Entity
  { entityID :: NamedID "_id" a e
  , entity :: e a }
  deriving stock Generic

deriving instance (FromJSON (e a), J.FromJSON (NamedID "_id" a e)) => FromJSON (Entity e a)
deriving instance (ToJSON (e a), J.ToJSON (NamedID "_id" a e)) => ToJSON (Entity e a)

type family NamedID n a (e :: * -> *) :: * where
    NamedID n Schema          e = Named n
    NamedID n Filter          e =   [ID e]
    NamedID n (Front Display) e = e (Front Display)
    NamedID n a               e =    ID e
{-
x :: forall e n. Known Symbol n => ((Entity e Schema -> e Schema) -> Named n) -> Named n
x f = f $ entity @e @Schema
-}

type family EntityOrID e a :: * where
    EntityOrID e Create          = ID e
    EntityOrID e (Front Create)  = ID e
    EntityOrID e Update          = ID e
    EntityOrID e (Front Display) = e (Front Display)

nameE :: forall (e :: * -> *) s. (Typeable e, IsString s) => s
nameE = let t = show (typeOf (Proxy @e)) 
        in fromString $ fromMaybe t $ stripPrefix "Proxy (* -> *) " t

fieldsE :: forall e. Data e  => [String]
fieldsE = concatMap constrFields . dataTypeConstrs . dataTypeOf $ (Proxy @e)

fieldsQuery :: forall e s. (Data e, IsString s) => s
fieldsQuery = fromString $ intercalate ", " $ fieldsE @e

{-}
fieldsE :: forall e (s :: *). (Data e, IsString s) => s
fieldsE = fromString 
        . intercalate ", " 
        . concatMap constrFields 
        . dataTypeConstrs 
        . dataTypeOf @e 
        $ undefined -}
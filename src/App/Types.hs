{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module App.Types where

import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Data
import Data.Kind (Type)
import Data.Function (on)
import Data.Text (Text)
import GHC.Generics

import qualified Data.Time as Time
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Simple.FromField (FromField)
import Data.String (IsString(..))
import Data.Maybe (fromMaybe)
import Data.List

type Body = BL.ByteString
type Page = Int
type Date = Time.Day
type PaginationSize = Int
type Token = Text


newtype ID e = ID { idVal :: Int }
  deriving stock (Generic, Data)
  deriving newtype (Read, ToField, FromField, FromJSON, ToJSON, Show, Eq, Ord, Num, Enum)
  deriving anyclass (FromRow)

type IDs = [ID (Path Current)]


nameOf :: forall (e :: Type -> Type) s. (Typeable e, IsString s) => s
nameOf = let t = show (typeOf (Proxy @e))
         in fromString $ fromMaybe t $ stripPrefix "Proxy (* -> *) " t

fieldsOf :: forall e. Data e => [String]
fieldsOf = concatMap constrFields . dataTypeConstrs . dataTypeOf $ (undefined :: e)

fieldsQuery :: forall e s. (Data e, IsString s) => s
fieldsQuery = fromString $ intercalate ", " $ fieldsOf @e
type URL = [Text]

data Path a
  = POST    URL 
  | GET     URL 
  | PUT     URL
  | DELETE  URL
  | Unknown URL
  deriving (Show, Eq, Generic, Typeable, Data)

getURL :: Path a -> URL
getURL = \case
    POST   t -> t
    GET    t -> t
    PUT    t -> t
    DELETE t -> t
    Unknown t -> t

type Method = forall a. [Text] -> Path a

getMethod :: Data a => Path a -> Method
getMethod = \case
    POST   _ -> POST
    GET    _ -> GET
    PUT    _ -> PUT
    DELETE _ -> DELETE
    Unknown _ -> Unknown

instance Eq ([Text] -> Path a) where
    (==) = (==) `on` ($ [])

data Pattern deriving (Typeable, Data)
data Current deriving (Typeable, Data)
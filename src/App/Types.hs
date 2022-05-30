{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module App.Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Lazy qualified as BL
import Data.Char (toLower)
import Data.Data
  ( Data (dataTypeOf),
    Proxy (Proxy),
    Typeable,
    constrFields,
    dataTypeConstrs,
    typeOf,
  )
import Data.Kind (Type)
import Data.List (intercalate, isInfixOf, stripPrefix)
import Data.Maybe (fromMaybe)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Time qualified as Time
import Extended.Postgres
  ( FromField,
    FromRow,
    PGArray (PGArray),
    ToField (..),
    ToRow,
  )
import GHC.Generics (Generic)

type Body = BL.ByteString

type Page = Int

type Date = Time.Day

type PaginationSize = Int

type Token = Text

type ContentType = Text

newtype ID e = ID {idVal :: Int}
  deriving stock (Generic, Data)
  deriving newtype (Read, ToField, FromField, FromJSON, ToJSON, Show, Eq, Ord, Num, Enum)
  deriving anyclass (FromRow, ToRow)

instance ToField [ID a] where
  toField a = toField $ PGArray a

nameOf :: forall (e :: Type -> Type) s. (Typeable e, IsString s) => s
nameOf =
  let t = show (typeOf (Proxy @e))
   in fromString $ unCapitalize $ fromMaybe t $ stripPrefix "Proxy (* -> *) " t
  where
    unCapitalize (x : xs) = toLower x : xs
    unCapitalize xs = xs

withPluralEnding :: String -> String
withPluralEnding s
  | "y" `isInfixOf` s = init s <> "ies"
  | otherwise = s <> "s"

fieldsOf :: forall e. Data e => [String]
fieldsOf = concatMap constrFields . dataTypeConstrs . dataTypeOf $ (undefined :: e)

fieldsQuery :: forall e s. (Data e, IsString s) => s
fieldsQuery = fromString $ intercalate ", " $ fieldsOf @e

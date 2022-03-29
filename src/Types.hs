{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types where

import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy qualified as BL
import Data.Data
import Data.Function (on)
import Data.Text (Text)


import GHC.Generics






import qualified Extended.Text as T
import qualified Data.Time as Time
import Text.Read (readMaybe)

type Body = BL.ByteString

type Page = Int

type Date = Time.Day

type PaginationSize = Int

newtype ID (a :: * -> *) = ID { idVal :: Int }
  deriving stock (Show, Generic)
  deriving newtype Read

instance FromJSON (ID a) where
  parseJSON = \case 
      String s -> case readMaybe $ T.unpack s of
        Just i -> pure $ ID i
        Nothing -> err (String s)
      a -> err a
    where err a = parseFail $ "failed to parse ID. Got: " <> show a
  
instance ToJSON (ID a) where
  toJSON (ID a) = String $ T.pack $ show a

type URL = [Text]

data Path a
  = POST   URL 
  | GET    URL 
  | PUT    URL
  | DELETE URL
  deriving (Show, Eq, Generic, Typeable, Data)

getURL :: Path a -> URL
getURL = \case
    POST   t -> t
    GET    t -> t
    PUT    t -> t
    DELETE t -> t

type Method = forall a. [Text] -> Path a

getMethod :: Data a => Path a -> Method
getMethod = \case
    POST   _ -> POST
    GET    _ -> GET
    PUT    _ -> PUT
    DELETE _ -> DELETE

instance Eq ([Text] -> Path a) where
    (==) = (==) `on` ($ [])


data Pattern deriving (Typeable, Data)
data Current deriving (Typeable, Data)
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module App.Path where

import App.Types (ID)
import Data.Data (Data, Typeable)
import Data.Function (on)
import Extended.Text (Text)
import GHC.Generics (Generic)
import Network.Wai qualified as Wai

type URL = [Text]

data Path a
  = POST URL
  | GET URL
  | PUT URL
  | DELETE URL
  | PUBLISH URL
  | Unknown URL
  deriving (Show, Eq, Generic, Typeable, Data)

toPath :: Wai.Request -> Path a
toPath req =
  let path = Wai.pathInfo req
   in case Wai.requestMethod req of
        "POST" -> POST path
        "GET" -> GET path
        "PUT" -> PUT path
        "DELETE" -> DELETE path
        "PUBLISH" -> PUBLISH path
        _ -> Unknown path

getURL :: Path a -> URL
getURL = \case
  POST t -> t
  GET t -> t
  PUT t -> t
  DELETE t -> t
  PUBLISH t -> t
  Unknown t -> t

type Method = forall a. [Text] -> Path a

getMethod :: Data a => Path a -> Method
getMethod = \case
  POST _ -> POST
  GET _ -> GET
  PUT _ -> PUT
  DELETE _ -> DELETE
  PUBLISH _ -> PUBLISH
  Unknown _ -> Unknown

instance Eq (URL -> Path a) where
  (==) = (==) `on` ($ [])

data Pattern deriving (Typeable, Data)

data Current deriving (Typeable, Data)

type IDs = [ID (Path Current)]

{-# LANGUAGE DerivingStrategies #-}

module Database.Config where

import App.Types (PaginationSize)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Dhall (FromDhall (..))
import Extended.Text qualified as T
import GHC.Generics (Generic)

data Config = Config
  { cHost :: !Text,
    cPort :: !Int,
    cUser :: !Text,
    cPassword :: !Text,
    cPagSize :: !PaginationSize
  }
  deriving (Show, Generic, FromDhall)

toDBConnectionString :: Config -> ByteString
toDBConnectionString Config {..} =
  T.encodeUtf8 $
    mconcat
      [ "host=",
        cHost,
        " port=",
        T.show cPort,
        " user=",
        cUser,
        " password=",
        cPassword
      ]

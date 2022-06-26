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
    cDatabase :: !Text,
    cPagSize :: !PaginationSize
  }
  deriving (Show, Generic, FromDhall)

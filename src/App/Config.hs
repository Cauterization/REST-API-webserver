module App.Config where

import Data.Aeson
import Database.Config qualified as Database
import Extended.Text (Text)
import GHC.Generics
import Logger qualified

type Port = Int

data Config = Config
  { cDatabase :: !Database.Config,
    cLogger :: !Logger.Config,
    cPort :: !Port,
    cAddress :: !Text
  }
  deriving (Show, Generic, FromJSON)

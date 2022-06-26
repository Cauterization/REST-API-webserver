module App.Config where

import Database.Config qualified as Database
import Dhall (FromDhall (..))
import Extended.Text (Text)
import GHC.Generics (Generic)
import Logger qualified

type Port = Int

data Config = Config
  { cDB :: !Database.Config,
    cLogger :: !Logger.Config,
    cPort :: !Port,
    cAddress :: !Text
  }
  deriving (Show, Generic, FromDhall)

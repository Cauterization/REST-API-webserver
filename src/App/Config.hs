module App.Config where

import Data.Aeson ( FromJSON )
import Database.Config qualified as Database
import Deriving.Aeson (CustomJSON (..), FieldLabelModifier, StripPrefix)
import Extended.Text (Text)
import GHC.Generics ( Generic )
import Logger qualified

type Port = Int

data Config = Config
  { cDatabase :: !Database.Config,
    cLogger :: !Logger.Config,
    cPort :: !Port,
    cAddress :: !Text
  }
  deriving (Show, Generic)
  deriving FromJSON via (CustomJSON '[FieldLabelModifier (StripPrefix "c")] Config) 

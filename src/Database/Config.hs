{-# LANGUAGE DerivingStrategies #-}

module Database.Config where

import App.Types ( PaginationSize )
import Data.Aeson ( FromJSON )
import Data.Text (Text)
import Deriving.Aeson (CustomJSON (..), FieldLabelModifier, StripPrefix)
import GHC.Generics ( Generic )

data Config = Config
  { cConn :: !Text,
    cPagSize :: !PaginationSize
  }
  deriving (Show, Generic)
  deriving FromJSON via (CustomJSON '[FieldLabelModifier (StripPrefix "c")] Config) 

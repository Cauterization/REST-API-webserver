{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Database.Config where

import App.Types
import Data.Aeson
import Data.Text (Text)
import GHC.Generics

data Config = Config
  { cConn :: Text,
    cPagSize :: PaginationSize
  }
  deriving (Show, Generic, FromJSON)

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Database.Config where

import App.Types (PaginationSize)
import Data.Aeson (FromJSON)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Deriving.Aeson (CustomJSON (..), FieldLabelModifier, StripPrefix)
import Extended.Text qualified as T
import GHC.Generics (Generic)

data Config = Config
  { cHost :: !Text,
    cPort :: !Int,
    cUser :: !Text,
    cPassword :: !Text,
    cPagSize :: !PaginationSize
  }
  deriving (Show, Generic)
  deriving (FromJSON) via (CustomJSON '[FieldLabelModifier (StripPrefix "c")] Config)

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

{-# LANGUAGE ImportQualifiedPost #-}

module Logger where

import Data.Time qualified as Time
import Dhall (FromDhall (..))
import Extended.Text qualified as T
import GHC.Generics (Generic)
import Prelude hiding (error, log)

data Verbosity
  = Sql
  | Debug
  | Info
  | Warning
  | Error
  deriving (Eq, Ord, Show, Generic, FromDhall)

data Mode
  = None
  | Display
  | Write
  | Both
  deriving (Eq, Ord, Show, Generic, FromDhall)

data Config = Config
  { cVerbosity :: !Verbosity,
    cMode :: !Mode,
    cFilePath :: !FilePath
  }
  deriving (Show, Generic, FromDhall)

type Logger m = Verbosity -> T.Text -> m ()

sql, debug, info, warning, error :: HasLogger m => T.Text -> m ()
sql = mkLog Sql
debug = mkLog Debug
info = mkLog Info
warning = mkLog Warning
error = mkLog Error

class HasLogger m where
  mkLog :: Logger m

(.<) :: (Show a) => T.Text -> a -> T.Text
text .< a = text <> T.show a

infixr 7 .<

(>.) :: (Show a) => a -> T.Text -> T.Text
(>.) = flip (.<)

infixr 7 >.

fromConfig :: Config -> Logger IO
fromConfig Config {..} v t
  | v < cVerbosity = pure ()
  | otherwise = format >>= toOutput
  where
    format = do
      utcTime <- Time.getCurrentTime
      let localTime = Time.addUTCTime (10800 :: Time.NominalDiffTime) utcTime
          asctime = Time.formatTime Time.defaultTimeLocale "%a %b %d %H:%M:%S %Y" localTime
      pure $ "\n\n" <> T.pack asctime <> " " .< v <> "\n" <> t
    toOutput out = case cMode of
      None -> pure ()
      Display -> T.putStrLn out
      Write -> T.appendFile cFilePath out
      Both -> T.putStrLn out >> T.appendFile cFilePath out

module Logger
  ( debug,
    info,
    warning,
    error,
    Config (..),
    Logger,
    Verbosity (..),
    Mode (..),
    HasLogger (..),
    RunLogger (..),
    (.<),
    (>.),
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson hiding (Error)
import Data.Time qualified as Time
import Extended.Text qualified as T
import GHC.Generics (Generic)
import Prelude hiding (error, log)

data Verbosity
  = Debug
  | Info
  | Warning
  | Error
  deriving (Eq, Ord, Show, Generic)

instance FromJSON Verbosity

data Mode
  = None
  | Display
  | Write
  | Both
  deriving (Eq, Ord, Show, Generic)

instance FromJSON Mode

data Config = Config
  { cVerbosity :: Verbosity,
    cMode :: Mode,
    cFilePath :: FilePath
  }
  deriving (Show, Generic)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> do
    cVerbosity <- o .: "Verbosity"
    cMode <- o .: "Mode"
    cFilePath <- o .: "FilePath" <|> pure "log.txt"
    pure Config {..}

type Logger m = Verbosity -> T.Text -> m ()

debug, info, warning, error :: (HasLogger m) => T.Text -> m ()
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

class RunLogger m where
  runLogger :: Config -> Logger m
  default runLogger :: MonadIO m => Config -> Logger m
  runLogger c = (liftIO .) . runLogger @IO c

instance RunLogger IO where
  runLogger Config {..} v t = do
    when (v >= cVerbosity) $ formatted >>= toOutput
    where
      formatted = do
        utcTime <- Time.getCurrentTime
        let localTime = Time.addUTCTime (10800 :: Time.NominalDiffTime) utcTime
            asctime =
              Time.formatTime
                Time.defaultTimeLocale
                "%a %b %d %H:%M:%S %Y"
                localTime
        pure $ "\n\n" <> T.pack asctime <> " " .< v <> "\n" <> t
      toOutput out = case cMode of
        None -> pure ()
        Display -> T.putStrLn out
        Write -> T.appendFile cFilePath out
        Both -> T.putStrLn out >> T.appendFile cFilePath out
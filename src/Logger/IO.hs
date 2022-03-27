module Logger.IO 
    ( fromConfig
    ) where

import           Data.Time 

import qualified Extended.Text as T
-- import qualified Prelude (error)
-- import           Prelude hiding (log, error)
-- import           System.Directory ( doesFileExist )
-- import           System.IO (withFile, IOMode(..))
-- import qualified System.IO as Sys

import Logger.Handle
import Control.Monad.IO.Class

fromConfig :: Config -> Logger IO
fromConfig Config{..} v t
    | v < cVerbosity = pure ()
    | otherwise = liftIO $ formatted >>= T.putStrLn
  where
    formatted = do 
        utcTime <- getCurrentTime
        let localTime = addUTCTime (10800 :: NominalDiffTime) utcTime
            asctime = formatTime defaultTimeLocale "%a %b %d %H:%M:%S %Y" localTime
        pure $ "\n\n" <> T.pack asctime <> " " <> T.show v <> "\n" <> t

{-}
withHandle :: Config -> (Handle IO -> IO ()) -> IO ()
withHandle Config{..} f 
    | cMode <= Display = f $ Handle Config{..} Nothing (doLog Config{..} Nothing)
    | otherwise = do
      fileExist <- doesFileExist cFilePath
      let f' = if fileExist then f else logFileAbsenceThenF
      withFile cFilePath AppendMode 
          (f' . \sysH -> Handle Config{..} (Just sysH) (doLog Config{..} (Just sysH)))
  where
    logFileAbsenceThenF h = warning h warn >> f h
    warn = "File " <> T.show cFilePath <> " doesn't exist. New one has been created."

doLog :: Config -> Maybe Sys.Handle -> Logger IO
doLog Config{..} fileH v t 
    | v < cVerbosity || cMode == None = return ()
    | cMode == Display = formated >>= T.putStrLn
    | cMode == Write   = formated >>= writeToFile
    | otherwise = do formated >>= T.putStrLn
                     formated >>= writeToFile
  where
    formated = do 
        utcTime <- getCurrentTime
        let localTime = addUTCTime (10800 :: NominalDiffTime) utcTime
            asctime = formatTime defaultTimeLocale "%a %b %d %H:%M:%S %Y" localTime
        pure $ "\n\n" <> T.pack asctime <> " " <> T.show v <> "\n" <> t
    writeToFile =  maybe (Prelude.error "No file handle") T.hPutStrLn  fileH  -}
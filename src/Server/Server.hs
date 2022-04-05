module Server.Server where

import Control.Exception ( IOException ) 
import Control.Monad
import Control.Monad.Catch
-- import Control.Monad.Writer

import Data.Aeson
import Data.ByteString.Lazy qualified as BL

-- import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Wai

import System.Exit qualified as Sys

-- import Types

-- import Extended.Text qualified as T
-- import Entities.Author
-- import Entities.User

import Server.App
import Server.Base
import Server.Run
-- import Server.Router  
-- import Server.Router qualified as Router

-- import Logger.Handle qualified as Logger
import Logger.IO qualified as Logger
import Database.Database qualified as Database
import Postgres.Postgres (Postgres)
-- import Database.Postgres (Postgres)

runServer ::  (?runMigrations :: Bool) => IO ()
runServer = handle handler $ do
    conf <- BL.readFile "config.json" >>= parseOrFail
    let logger = Logger.fromConfig $ cLogger conf
    when ?runMigrations $ Database.runMigrations @Postgres (cDatabase conf) logger
    Wai.run 3000 $ \req respond -> do
        body <- Wai.strictRequestBody req
        ToResponse{..} <- runRouter conf req body
        respond $ Wai.responseLBS respStatus respHeaders respBody
  where
    handler = \(e :: IOException) -> Sys.die $ show e <> ". Terminating..."
    parsingFail = fail . ("Parsing config error: " <>) . show 
    parseOrFail = either parsingFail pure . eitherDecode




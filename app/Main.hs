module Main where

import Control.Exception ( IOException ) 
import Control.Monad
import Control.Monad.Catch
-- import Control.Monad.Writer

import Data.Aeson
import Data.ByteString.Lazy qualified as BL

-- import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Wai

import System.Environment
import System.Exit qualified as Sys

import App.App qualified  as App
import App.Endpoints qualified  as App

import Database.Database qualified as Database

import Logger qualified
import Logger ((.<))

main :: IO ()
main = getArgs >>= \case    
    "migrations":_ -> let ?runMigrations = True in runServer
    _              -> let ?runMigrations = True in runServer

runServer ::  (?runMigrations :: Bool) => IO ()
runServer = handle handler $ do
    App.Config{..} <- BL.readFile "config.json" >>= parseOrFail
    connectionDB <- Database.mkConnectionIO @(App.DB IO) cDatabase
    let logger = Logger.runLogger @IO cLogger
    -- when ?runMigrations $ Database.runMigrations @Postgres (cDatabase conf) logger
    Wai.run 3000 $ \req respond -> do
        body <- Wai.strictRequestBody req
        App.ToResponse{..} <- App.runRouterWith
            logger 
            connectionDB
            (App.toPath req) 
            body
            (App.toQueryParams $ Wai.queryString req)
            (Database.cPagSize cDatabase)
            (Logger.debug $ "Recieved request:\n" .< req)
        respond $ Wai.responseLBS respStatus respHeaders respBody
  where
    handler = \(e :: IOException) -> Sys.die $ show e <> ". Terminating..."
    parsingFail = fail . ("Parsing config error: " <>) . show 
    parseOrFail = either parsingFail pure . eitherDecode
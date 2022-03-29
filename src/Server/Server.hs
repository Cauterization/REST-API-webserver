module Server.Server where

import Control.Exception ( IOException ) 
import Control.Monad.Reader
import Control.Monad.Catch
import Control.Monad.Writer

import Data.Aeson
import Data.ByteString.Lazy qualified as BL

import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Wai

import System.Exit qualified as Sys

import Types

import Extended.Text qualified as T
import Entities.Author
import Entities.User

import Server.App
import Server.Base
import Server.Router  
import Server.Router qualified as Router

import Logger.Handle qualified as Logger
import Logger.IO qualified as Logger
import Database.Database qualified as Database
import Database.Postgres (Postgres)

runServer ::  (?runMigrations :: Bool) => IO ()
runServer = handle handler $ do
    conf <- BL.readFile "config.json" >>= parseOrFail
    conn <- Database.mkConnectionIO @App $ cDatabase conf
    let logger = Logger.fromConfig $ cLogger conf
    when ?runMigrations $ Database.runMigrations @App logger conn
    Wai.run 3000 $ \req respond -> do
        body <- Wai.strictRequestBody req
        ToResponse{..} <- runRouter conf req body
        respond $ Wai.responseLBS respStatus respHeaders respBody
  where
    handler = \(e :: IOException) -> Sys.die $ show e <> ". Terminating..."
    parsingFail = fail . ("Parsing config error: " <>) . show 
    parseOrFail = either parsingFail pure . eitherDecode

data ToResponse 
    = ToResponse 
    { respStatus  :: HTTP.Status
    , respHeaders :: [HTTP.Header] 
    , respBody    :: Body
    } deriving Show

runRouter :: Config -> Wai.Request -> Body -> IO ToResponse
runRouter conf req body 
    = fmap fromResult 
    $ flip catches handler 
    $ runApp conf req body $ do 
        Logger.debug $ "Request received:\n" <> T.show req
        toPath req >>= execWriterT . runReaderT (unRouter (router @Main)) >>= \case
            Route _ success      -> success
            AmbiguousPatterns ps -> throwM $ OtherErr $ "AmbiguousPatterns" <> T.show ps
            _                    -> throw404
  where
    handler = [Handler appH]
    appH = pure . fromErr
    fromResult = \case
        ResText t  -> r200text $ toBL t
        ResJSON bl -> r200json bl
        ResPicture -> error "runRouter picture result"
    r200text = ToResponse HTTP.status200 
        [("ContentType","text/plain; charset=utf-8")]
    r200json = ToResponse HTTP.status200 [("ContentType","application/json")]
    r404     = ToResponse HTTP.status404 [] "Not found."
    r400     = ToResponse HTTP.status400 []
    r500     = ToResponse HTTP.internalServerError500 [] "Internal error."
    toBL = BL.fromStrict . T.encodeUtf8

data Main :: * -> *

instance Router.Routed Main Postgres where
    router = do
        newRouter @User 
        newRouter @Author 




{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}

module App.App 
    ( runServer
    ) where

import App.Config
import App.Run
import App.Result
import App.Get
import App.Put
import App.Delete
import App.Router 
import App.Types
import App.Internal
import App.QueryParams

import Api.ProtectedResources (protectedResources)
import Api.Author qualified as Author
import Api.User qualified as User

import Control.Exception ( IOException ) 
import Control.Monad
import Control.Monad.Catch
-- import Control.Monad.Writer

import Data.Aeson
import Data.Kind
import Data.ByteString.Lazy qualified as BL

import Extended.Text qualified as T

-- import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Wai

import System.Environment
import System.Exit qualified as Sys

import Database.Database qualified as Database

import Logger qualified
import Logger ((.<))
import Entity.Author (Author)

import Entity.User (User)
import Postgres.Internal

import Control.Monad.Extra (whenM)
import Data.Functor ((<&>))

runServer :: IO ()
runServer = handle handler $ do
    Config{..} <- BL.readFile "config.json" >>= parseOrFail
    connectionDB <- Database.mkConnectionIO @(DB IO) cDatabase
    let logger = Logger.runLogger @IO cLogger
    whenM (getArgs <&> (== ["migrations"])) 
        $ Database.runMigrations @(DB IO) cDatabase logger
    Database.runMigrations @(DB IO) cDatabase logger -- DEBUG
    Wai.run 3000 $ \req respond -> do
        body <- Wai.strictRequestBody req
        ToResponse{..} <- runRouterWith @Main
            logger 
            connectionDB
            (toPath req) 
            body
            (toQueryParams $ Wai.queryString req)
            (T.decodeUtf8 <$> lookup "Authorization" (Wai.requestHeaders req))
            (Database.cPagSize cDatabase)
            (Logger.debug $ T.take 500 $ "Recieved request:\n" .< req)
        respond $ Wai.responseLBS respStatus respHeaders respBody
  where
    handler (e :: IOException) = Sys.die $ show e <> ". Terminating..."
    parsingFail = fail . ("Parsing config error: " <>) . show 
    parseOrFail = either parsingFail pure . eitherDecode

data Main :: Type -> Type

instance Routed Main Postgres where
    router = do
        addMiddleware protectedResources
        newRouter @User 
        newRouter @Author 
        
instance Routed User Postgres where
    router = do
        post    "users"            User.postUser
        get     "users/me"         User.getCurrentUser
        delete_ "admin/users/{ID}" 
        post    "auth"            User.loginUser

instance Routed Author Postgres where
    router = do
        post    "admin/authors"    Author.postAuthor   
        get_    "admin/authors"          
        --get_    "admin/authors/{ID}" 
        put_    "admin/authors/{ID}"     
        delete_ "admin/authors/{ID}"  
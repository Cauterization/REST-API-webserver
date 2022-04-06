{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}

module App.Internal where

import Control.Monad.Catch 
import Control.Monad.Reader 
import Control.Monad.IO.Class

import Data.Kind (Type)
import Extended.Text (Text)
import Extended.Text qualified as T
import App.Config qualified as App
import App.QueryParams

import App.Types

import Logger qualified 
import qualified Network.HTTP.Types as HTTP
import qualified Data.Map as M
import Control.Arrow ((>>>))
import Data.Maybe
import Data.Functor
import Data.Data (Typeable)
import qualified Network.Wai as Wai

import Database.Database qualified as Database

import Database.PostgreSQL.Simple
import Postgres.Internal

newtype AppT m a = App {unApp :: ReaderT (Env m) m a}
    deriving newtype 
        ( Functor
        , Applicative
        , Monad
        , MonadReader (Env m)
        )

type DB m = Database.Database (AppT m)

deriving newtype instance MonadThrow m => MonadThrow (AppT m)
deriving newtype instance MonadIO (AppT IO)
deriving newtype instance MonadCatch (AppT IO)
deriving anyclass instance MonadIO (AppT m) => Logger.RunLogger (AppT m)
-- deriving instance MonadIO m => MonadThrow (AppT m)

instance MonadTrans AppT where
    lift = App . lift

instance Monad m => Logger.HasLogger (AppT m) where
    mkLog v t = asks envLogger >>= \l -> lift $ l v t 

instance Database.HasDatabase (AppT IO) where

    type Database (AppT IO) = Postgres

    liftDatabase = liftIO

    getDatabaseConnection = asks envConn

    getPaginationSize = asks envPagination


data Env (m :: Type -> Type) = Env
    { envLogger     :: Logger.Logger m
    , envConn       :: Database.ConnectionOf (Database.Database (AppT m))
    , envPath       :: Path Current
    , envBody       :: Body
    , envQParams    :: QueryParams
    , envPagination :: PaginationSize
    }
   
type family EnvOf a where
    EnvOf (AppT m) = m 

type HasEnv m = MonadReader (Env (EnvOf m)) m

getPath :: HasEnv m => m (Path Current)
getPath = asks envPath

getParam :: (HasEnv m, MonadThrow m) => Text -> m (Maybe Text)
getParam p = asks (M.lookup p . envQParams) >>= \case
    Just [a] -> pure $ Just a
    Nothing -> pure Nothing
    _ -> throwM $ QParamsErr $ "multiparam " <> p 

getPage :: (Monad m, MonadThrow m, Logger.HasLogger m, HasEnv m)  => m Page
getPage = getParam "page" <&> (fromMaybe "1" >>> T.read) >>= either
    (parsingError . ("page: " <>) .  T.unpack)
    (\n -> if n > 0 then pure n else throwM $ QParamsErr "zero or negative page")

toPath ::  Wai.Request -> Path a
toPath req = 
    let path = Wai.pathInfo req 
    in case Wai.requestMethod req of
        "POST"   -> POST    path
        "GET"    -> GET     path
        "PUT"    -> PUT     path
        "DELETE" -> DELETE  path
        unknown  -> Unknown path

data AppError 
    = Err404 (Path Current)
    | ParsingErr Text
    | UnknwonHTTPMethod HTTP.Method
    | QParamsErr Text
    | OtherErr Text
    | ArityError Text
    deriving (Show, Typeable, Exception) 

parsingError :: (MonadThrow m, Logger.HasLogger m) => String -> m a
parsingError = throwM . ParsingErr . T.pack 

throw404 :: (MonadThrow m, HasEnv m, Logger.HasLogger m) =>  m a
throw404 = getPath >>= throwM . Err404 
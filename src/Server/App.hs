{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Server.App where

import Control.Monad.Catch 

import Control.Monad.Reader 

import Data.Aeson ( ToJSON )

import Entities.Internal (nameE)

import HKD.Display


import Network.Wai qualified as Wai

import Logger.Handle qualified as Logger
import Logger.IO qualified as Logger

import Database.Database ( Database, HasDatabase(..))
import Database.Database qualified as Database
import Postgres.Postgres (Postgres)

import HKD.Front (Front)

import Server.Base hiding (Env)
import Server.Base qualified as Base
import Types

newtype App a = App {unApp :: ReaderT Env IO a}
    deriving newtype 
        ( Functor
        , Applicative
        , Monad
        , MonadReader Env
        , MonadIO
        , MonadThrow
        )

data Env = Env
    { envConn   :: Database.Connection Postgres
    , envLogger :: Logger.Logger App
    , envBase   :: Base.Env
    }

instance Logger.HasLogger App where
    mkLog v t = do
        l <- asks envLogger
        l v t

runApp :: Config -> Wai.Request -> Body -> App a -> IO a
runApp conf req body app = do
    env <- toEnv conf req body
    conn <- Database.mkConnectionIO @Postgres $ cDatabase conf
    let logger = Logger.fromConfig $ cLogger conf
    flip runReaderT 
        (Env conn ((liftIO . ) . logger) env)
        $ unApp $ do
        app

instance HasDatabase App where
    type Database App = Postgres

instance HasEnv App where
    asksEnv f = asks (f . envBase)

instance Database.HasConnection App where
    getConn = asks envConn

type Application m = 
    ( Monad m
    , Logger.HasLogger m
    , HasDatabase m
    , MonadThrow m
    , HasEnv m
    )

type Entity db e =
    ( Database.DBEntity db e
    , ToJSON (e (Front Display))
    )

getE :: forall (e :: * -> *) m id. 
    ( GettableFrom (Database m) e
    , ToJSON (e (Front Display))
    , Application m
    , GArgs e ~ Page
    ) => [ID Path] -> m AppResult
getE _ = do
    Logger.info $ "Attempt to get " <> nameE @e
    entities <- Database.getEFromDBDefault id @_ @e =<< getPage
    Logger.info $ nameE @e <> " was found."
    json entities

getEByID :: forall (e :: * -> *) m id. 
    ( Database.GettableSingleFrom (Database m) e
    , ToJSON (e (Front Display))
    , Application m
    , GArgs e ~ ID Path
    ) => [ID Path] -> m AppResult
getEByID ids = do
    Logger.info $ "Attempt to get " <> nameE @e
    entities <- Database.getEFromDBDefault id @_ @e =<< Database.toGArgs @e ids
    Logger.info $ nameE @e <> " was found."
    json entities
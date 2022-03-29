{-# LANGUAGE DeriveDataTypeable #-}
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
import Database.Postgres (Postgres)

import HKD.Front (Front)

import Server.Base hiding (Env)
import Server.Base qualified as Base
import Types




newtype App a = App {unApp :: ReaderT Env IO a}
    deriving newtype 
        (Functor
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

runApp :: (?runMigrations :: Bool)
    => Config 
    -> Wai.Request 
    -> Body 
    -> App a 
    -> IO a
runApp conf req body app = do
    conn <- Database.mkConnectionIO @App $ cDatabase conf
    env <- toEnv conf req body
    let logger = Logger.fromConfig $ cLogger conf
    flip runReaderT 
        (Env conn ((liftIO . ) . logger) env)
        $ unApp $ do
        when ?runMigrations (liftIO $ Database.runMigrations @App logger conn)
        app

type instance Database App = Postgres

instance HasEnv App where
    asksEnv f = asks (f . envBase)

instance HasDatabase App where
    getConnection = asks envConn

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
    ( Entity (Database m) e
    , Application m
    ) => [ID Path] -> m AppResult
getE ids = do
    Logger.info $ "Attempt to get " <> nameE @e
    entities <- do
        conn <- Database.getConnection
        Database.getE @_ @e conn ids =<< getPage
    Logger.info $ nameE @e <> "was found."
    json entities
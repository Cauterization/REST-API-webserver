{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Postgres.Postgres  
    ( Postgres
    ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader

import Data.Pool qualified as Pool


import Database.PostgreSQL.Simple

import Data.ByteString qualified as B
import Data.Function (on)
import Data.FileEmbed
import Data.List qualified as L
import Data.Text.Encoding qualified as T
import Data.String


import Entities.Internal
import Extended.Text qualified as T

import Database.Database qualified as Database


import Database.PostgreSQL.Simple.Migration (MigrationContext(..), MigrationResult(..), MigrationCommand (MigrationInitialization, MigrationScript), runMigration)
import Database.PostgreSQL.Simple.ToField

import System.Exit 

import HKD.Display
import HKD.Front (Front)
import HKD.Schema

import Types

import Logger.Handle qualified as Logger
import Data.Data (Data)
import Database.Error (DBErr(EntityNotFound))

data Postgres :: *

type instance Database.Connection Postgres = Pool.Pool Connection

type instance Database.FromRow Postgres = FromRow

type instance Database.ToField Postgres = ToField

type instance Database.Query Postgres = Query

newtype DBArgs m a = DBArgs {unDBA :: ReaderT IDs m a}
    deriving newtype (Functor, Applicative, Monad, MonadReader IDs, MonadThrow) 

instance (Monad m, MonadThrow m) => MonadFail (DBArgs m) where
    fail _ = throwM $ EntityNotFound ""

getIDs :: Monad m => DBArgs m IDs
getIDs = ask 

instance Database.IsDatabase Postgres where
    
    type DBConstraints Postgres m = 
        ( MonadIO m
        , Logger.HasLogger m
        , Database.HasConnection m
        )

    mkConnectionIODB Database.Config{..} = Pool.createPool
        (connectPostgreSQL $ T.encodeUtf8 cConn)
        close
        1
        30
        4

    runMigrationsDB conf l = do
        pool <- Database.mkConnectionIODB @Postgres conf
        Pool.withResource pool $ \conn -> do
            let defaultContext = 
                    MigrationContext
                    { migrationContextCommand = MigrationInitialization
                    , migrationContextVerbose = False
                    , migrationContextConnection = conn
                    }
                migrations = ("(init)", defaultContext) :
                             [
                                (k, defaultContext
                                    { migrationContextCommand =
                                        MigrationScript k v
                                    })
                                | (k, v) <- sortedMigrations
                             ]
            forM_ migrations $ \(migrDescr, migr) -> do
                l Logger.Info $ "Running migration: " <> T.pack migrDescr
                res <- runMigration migr
                case res of
                    MigrationSuccess -> return ()
                    MigrationError reason -> do
                        l Logger.Error $ "Migration failed: " <> T.pack reason
                        exitFailure

    getEDefaultDB :: forall m e id. 
        ( Database.Database m ~ Postgres
        , Database.DBEntity (Database.Database m) e
        , Database.HasPagSize m
        , Database.MConstraints m
        ) 
        => [ID id] 
        -> Page 
        -> m [e (Front Display)] 
    getEDefaultDB _ page = do
        pc <- Database.getConn
        pagination <- fromString . show <$> Database.getPagSize
        let q = Database.unEQuery $ 
                Database.getEQuery @Postgres @e @(Front Display)
                <> Database.qconcat 
                    [ " LIMIT " , pagination
                    , " OFFSET ", pagination, " * (? - 1)"]
        Logger.debug $ T.show q
        liftIO $ Pool.withResource pc $ \conn -> query conn q (Only page)

    getEByIDDefaultDB :: forall m e id. 
        ( Database.Database m ~ Postgres
        , Database.DBEntity (Database.Database m) e
        , Database.MConstraints m
        , Database.ToField Postgres (ID id)
        ) 
        => [ID id] 
        -> m (e (Front Display))
    getEByIDDefaultDB [eID] = do
        pc <- Database.getConn
        let q = Database.unEQuery $ 
                Database.getEQuery @Postgres @e @(Front Display)
                <> "WHERE id = ?"
        Logger.debug $ T.show q
        liftIO $ Pool.withResource pc $ \conn -> query conn q (Only eID) >>= \case
            []   -> throwM $ EntityNotFound $ nameE @e
            x:xs -> pure x 

    getEByIDQueryDefault :: forall (e :: * -> *) (a :: *).
        (Database.DBEntity Postgres e, Data (e a))
        => Database.EQuery Postgres (e a)
    getEByIDQueryDefault = Database.getEQuery @Postgres @e @a
        <> "WHERE id = ?"

sortedMigrations :: [(FilePath, B.ByteString)]
sortedMigrations =
  let unsorted = $(embedDir "migrations")
  in L.sortBy (compare `on` fst) unsorted


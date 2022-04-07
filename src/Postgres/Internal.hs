{-# LANGUAGE TemplateHaskell #-}

module Postgres.Internal where

import Control.Monad

import  Data.ByteString qualified as B
import  Data.List qualified as L
import  Data.Function (on)
import Data.Pool qualified as Pool
import Data.FileEmbed 

import Extended.Text qualified as T

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration
import Database.PostgreSQL.Simple.ToField

import System.Exit 

import Database.Database (Database, Config, IsDatabase)
import Database.Database qualified as Database

import Logger qualified

data Postgres -- undefined

instance IsDatabase Postgres where

    type QueryOf Postgres = Query
    type ToRowOf Postgres q = ToRow q
    type FromRowOf Postgres r = FromRow r
    type ConnectionOf Postgres = Pool.Pool Connection
    type DatabaseMonad Postgres = IO

    mkConnectionIO Database.Config{..} = Pool.createPool
        (connectPostgreSQL $ T.encodeUtf8 cConn)
        close
        1
        30
        4

    runMigrations conf l = do
        pool <- Database.mkConnectionIO @Postgres conf
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

    postToDatabase pc q a = void 
        $ Pool.withResource pc $ \conn -> execute conn q a

    getFromDatabase pc q a =  Pool.withResource pc $ \conn -> query conn q a

    putIntoDatabase pc q a = void 
        $ Pool.withResource pc $ \conn -> execute conn q a
 
    deleteFromDatabase pc q a = fmap fromIntegral 
        $ Pool.withResource pc $ \conn -> execute conn q a

sortedMigrations :: [(FilePath, B.ByteString)]
sortedMigrations =
  let unsorted = $(embedDir "migrations")
  in L.sortBy (compare `on` fst) unsorted
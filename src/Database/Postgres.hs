{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Database.Postgres  
    ( Postgres
    ) where

import Control.Monad.IO.Class
import Control.Monad.Reader

import Data.Pool qualified as Pool


import Database.PostgreSQL.Simple

import Data.ByteString qualified as B
import Data.Function (on)
import Data.FileEmbed
import Data.List qualified as L
import Data.Text.Encoding qualified as T
import Data.List (intercalate)
import Data.String


import Entities.Internal
import Extended.Text qualified as T

import Database.Config qualified as Database
import Database.Database qualified as Database

import Database.PostgreSQL.Simple.Migration (MigrationContext(..), MigrationResult(..), MigrationCommand (MigrationInitialization, MigrationScript), runMigration)

import System.Exit 

import HKD.Display

import Types

import Logger.Handle qualified as Logger

data Postgres :: *

type instance Database.Connection Postgres = Pool.Pool Connection

type instance Database.FromRow Postgres = FromRow


instance Database.IsDatabase Postgres where
    
    type DBConstraints Postgres m = MonadIO m

    mkConnectionIODB Database.Config{..} = Pool.createPool
        (connectPostgreSQL $ T.encodeUtf8 cConn)
        close
        1
        30
        4

    runMigrationsDB l pool = Pool.withResource pool $ \conn -> do
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
        , Database.Entity (Database.Database m) e
        , Database.HasPagSize m
        , MonadIO m
        ) => Database.Connection (Database.Database m)
        -> [ID id] 
        -> Page 
        -> m [e Display] 
    getEDefaultDB pc _ page = do
        pagination <- fromString . show <$> Database.getPagSize
        let q = mconcat 
                [ "SELECT "
                , fromString $ intercalate ", " $ fieldsE @(e Display)
                , " FROM ", nameE @e, "s_view "
                , " LIMIT ", pagination
                , " OFFSET ", pagination, " * (? - 1)"
                ] 
        liftIO $ Pool.withResource pc $ \conn -> query conn q (Only page)

sortedMigrations :: [(FilePath, B.ByteString)]
sortedMigrations =
  let unsorted = $(embedDir "migrations")
  in L.sortBy (compare `on` fst) unsorted


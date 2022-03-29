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
import HKD.Front (Front)

import Types

import Logger.Handle qualified as Logger


data Postgres :: *

type instance Database.Connection Postgres = Pool.Pool Connection

type instance Database.FromRow Postgres = FromRow

type instance Database.Query Postgres = Query

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
        , Database.DBEntity (Database.Database m) e
        , Database.HasPagSize m
        , Database.MConstraints m
        ) => Database.Connection (Database.Database m)
        -> [ID id] 
        -> Page 
        -> m [e (Front Display)] 
    getEDefaultDB pc _ page = do
        pagination <- show <$> Database.getPagSize
        let q = Database.unEQuery $ mconcat
                [ Database.getEQ @Postgres @e
                , " LIMIT " , fromString pagination
                , " OFFSET ", fromString pagination, " * (? - 1)"
                ] 
        liftIO $ Pool.withResource pc $ \conn -> query conn q (Only page)

    getEQDefault :: forall (e :: * -> *) (a :: *). 
        Database.DBEntity Postgres e 
        => Database.EQuery Postgres (e a)
    getEQDefault = mconcat 
        [ "SELECT "
        -- , fromString $ intercalate ", " $ fieldsE @(e a)
        , " FROM ", nameE @e, "s_view "
        ]
        

sortedMigrations :: [(FilePath, B.ByteString)]
sortedMigrations =
  let unsorted = $(embedDir "migrations")
  in L.sortBy (compare `on` fst) unsorted


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

instance Database.IsDatabase Postgres where
    
    type DBConstraints Postgres m = 
        ( MonadIO m
        , Logger.HasLogger m
        , Database.HasConnection m
        )

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

    -- getFromDB :: forall m. 
    --     (HasDatabase m, Database m ~ db, MConstraints m, ToField db (Args e))
    --     => (EQuery db (e (Front Display)) -> EQuery db (e (Front Display))) 
    --     -> Args e 
    --     -> m [e (Front Display)]
    -- default getFromDB :: forall m. 
    --     (HasDatabase m, Database m ~ db, MConstraints m, ToField db (Args e), Args e ~ Page)
    --     => (EQuery db (e (Front Display)) -> EQuery db (e (Front Display))) 
    --     -> Args e 
    --     -> m [e (Front Display)]
    -- getFromDB = getEFromDBDefault

    getEFromDBDefault :: forall m e a. 
        ( Database.HasDatabase m
        , Database.Database m ~ Postgres
        , Database.MConstraints m
        , Database.GettableFrom Postgres e
        , Data (e a)
        , FromRow (e a)
        , ToField (Database.GArgs e)
        ) => ( Database.EQuery Postgres (e a) 
            -> Database.EQuery Postgres (e a)) 
        -> Database.GArgs e 
        -> m [e a]
    getEFromDBDefault queryModifier arg = do 
        pc <- Database.getConn 
        let q = Database.unEQuery $ queryModifier $
                Database.getQuery @Postgres @e @a
        Logger.debug $ T.show q
        liftIO $ Pool.withResource pc $ \conn -> query conn q (Only arg)




    -- getEDefault :: forall m e. 
    --     ( Database.Database m ~ Postgres
    --     , Database.GettableManyFrom Postgres e
    --     , Database.HasPagSize m
    --     , Database.MConstraints m
    --     ) 
    --     => Page 
    --     -> m [e (Front Display)] 
    -- getEDefault page = do
    --     pc <- Database.getConn
    --     pagination <- fromString . show <$> Database.getPagSize
    --     let q = Database.unEQuery $ 
    --             Database.getEQuery @Postgres @e @(Front Display)
    --             <> Database.qLIMIT pagination
    --             <> Database.qOFFSET (pagination <> " * (? - 1)")
    --     Logger.debug $ T.show q
    --     liftIO $ Pool.withResource pc $ \conn -> query conn q (Only page)
    
    -- getEQueryDefault :: forall (e :: * -> *) (a :: *).
    --     (Database.DBEntity Postgres e, Data (e a))
    --     => Database.EQuery Postgres (e a)
    -- getEQueryDefault = Database.qSELECT (fieldsQuery @(e a))
    --                 <> Database.qFROM (nameE @e <> "s_view ")

    -- getEQueryDefault :: forall (e :: * -> *) (a :: *).
    --     (Database.DBEntity Postgres e, Data (e a))
    --     => Database.EQuery Postgres (e a)
    -- getEQueryDefault = Database.qconcat 
    --     [ "SELECT " <> fieldsQuery @(e a)
    --      <>  " FROM " <> nameE @e <> "s_view "
    --     ]

    -- getEByIDDefault :: forall m e. 
    --     ( Database.Database m ~ Postgres
    --     , Database.DBEntity (Database.Database m) e
    --     , Database.MConstraints m
    --     , Database.ToField Postgres (ID Path)
    --     , Database.FromRow Postgres (e (Front Display))
    --     ) 
    --     => ID Path
    --     -> m (e (Front Display))
    -- getEByIDDefault eID = do
    --     pc <- Database.getConn
    --     let q = Database.unEQuery $ Database.getEByIDQueryDefault @Postgres @e @(Front Display)
    --     Logger.debug $ T.show q
    --     liftIO $ Pool.withResource pc $ \conn -> query conn q (Only eID) >>= \case
    --         []   -> throwM $ EntityNotFound $ nameE @e
    --         x:xs -> pure x 

    -- getEByIDQueryDefault :: forall (e :: * -> *) (a :: *).
    --     (Database.DBEntity Postgres e, Data (e a))
    --     => Database.EQuery Postgres (e a)
    -- getEByIDQueryDefault = Database.getEQueryDefault @Postgres @e @a
        -- <> Database.qWHERE " id = ?"

sortedMigrations :: [(FilePath, B.ByteString)]
sortedMigrations =
  let unsorted = $(embedDir "migrations")
  in L.sortBy (compare `on` fst) unsorted

{-
>>> Database.qconcat @Postgres $ mconcat [ "SELECT ", "ASd", " FROM " <> "USers" <> "s_view "]
Found hole: _ :: EQuery Postgres e0 -> t
Where: ‘e0’ is an ambiguous type variable
       ‘t’ is a rigid type variable bound by
         the inferred type of it :: t
         at C:\Users\Teradied\Desktop\server\src\Postgres\Postgres.hs:165:2-95
Couldn't match type ‘Query’ with ‘Char’
  arising from the literal ‘"SELECT "’
-}

{-
>>> fromString @Query "SELECT FROM TO WHERE "
"SELECT FROM TO WHERE "
-}

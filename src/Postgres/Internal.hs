{-# LANGUAGE TemplateHaskell #-}

module Postgres.Internal where

import Control.Monad
import Control.Monad.Catch

import Data.ByteString qualified as B hiding (putStrLn)
-- import Data.ByteString.Char8 qualified as B
import Data.List qualified as L
import Data.Function (on)
import Data.Pool qualified as Pool
import Data.FileEmbed 

import Extended.Text qualified as T

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration

import System.Exit 

import Database.Internal (IsDatabase)
import Database.Internal qualified as Database
import Database.Config qualified as Database

import Logger qualified

data Postgres 

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

    postToDatabase pc q a = handleSql $ Pool.withResource pc $ \conn -> 
        formatQuery conn q a >>= B.putStr >>
        query conn q a >>= Database.getSingle 

    getFromDatabase pc q a = handleSql $ Pool.withResource pc $ \conn -> 
        formatQuery conn q a >>= B.putStr >>
        query conn q a

    putIntoDatabase pc q a = handleSql $ fmap fromIntegral  
        $ Pool.withResource pc $ \conn -> 
            formatQuery conn q a >>= B.putStr >>
            execute conn q a
 
    deleteFromDatabase pc q a = handleSql $ fmap fromIntegral 
        $ Pool.withResource pc $ \conn -> 
            formatQuery conn q a >>= B.putStr >>
            execute conn q a

sortedMigrations :: [(FilePath, B.ByteString)]
sortedMigrations =
  let unsorted = $(embedDir "migrations")
  in L.sortBy (compare `on` fst) unsorted

handleSql :: IO a -> IO a
handleSql = handle $ \SqlError{..} -> case sqlState of
        "23503" -> throwM $ Database.EntityNotFound $ T.decodeUtf8 sqlErrorDetail
        "23502" -> throwM $ Database.IsNull $ T.decodeUtf8 sqlErrorDetail
        "23505" -> throwM $ Database.AlreadyExists $ T.decodeUtf8 sqlErrorDetail
        _       -> throwM $ Database.UnknwonError $ T.show SqlError{..}

-- catchSQLE :: IO (Either DBError b) -> IO (Either DBError b)
-- catchSQLE = handle (\PG.SqlError{..} -> return $ Left $ toDbE $ parseSqlE PG.SqlError{..})
--   where 
--     toDbE = \case
--         CustomErrorP0001 t        -> AccessViolation t
--         ForeignKeyViolation t     -> EntityNotFound t
--         MultiUniqueKeys t         -> AlreadyExists t
--         NullConstraintViolation t -> IsNull t
--         UnknownSqlE t             -> UnknownDbE t
--     parseSqlE PG.SqlError{..} = case sqlState of
--         "23503" -> ForeignKeyViolation $ T.decodeUtf8 sqlErrorDetail
--         "23502" -> NullConstraintViolation $ T.decodeUtf8 sqlErrorDetail
--         "23505" -> MultiUniqueKeys  $ T.decodeUtf8 sqlErrorDetail
--         "P0001" -> CustomErrorP0001 $ T.decodeUtf8 sqlErrorMsg
--         _       -> UnknownSqlE $ T.showT PG.SqlError{..}

-- data SqlE = CustomErrorP0001 T.Text
--           | MultiUniqueKeys T.Text
--           | ForeignKeyViolation T.Text
--           | NullConstraintViolation T.Text
--           | UnknownSqlE T.Text
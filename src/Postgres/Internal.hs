{-# LANGUAGE TemplateHaskell #-}

module Postgres.Internal where

import Control.Monad (forM_)
import Control.Monad.Catch
  ( Handler (Handler),
    MonadThrow (throwM),
    catches,
  )
import Data.ByteString qualified as B hiding (putStrLn)
import Data.FileEmbed (embedDir)
import Data.Function (on)
import Data.List qualified as L
import Data.Pool qualified as Pool
import Database.Config qualified as Database
import Database.Internal (IsDatabase)
import Database.Internal qualified as Database
import Database.PostgreSQL.Simple.Migration
  ( MigrationCommand (MigrationInitialization, MigrationScript),
    MigrationContext
      ( MigrationContext,
        migrationContextCommand,
        migrationContextConnection,
        migrationContextVerbose
      ),
    MigrationResult (MigrationError, MigrationSuccess),
    runMigration,
  )
import Extended.Postgres
import Extended.Text qualified as T
import Logger qualified
import System.Exit (exitFailure)

data Postgres

instance IsDatabase Postgres where
  type QueryOf Postgres = Query
  type ToRowOf Postgres q = ToRow q
  type FromRowOf Postgres r = FromRow r
  type ConnectionOf Postgres = Pool.Pool Connection
  type DatabaseMonad Postgres = IO

  mkConnectionIO Database.Config {..} =
    Pool.createPool
      (connectPostgreSQL $ Database.toDBConnectionString Database.Config {..})
      close
      1
      30
      4

  runMigrations conf l = do
    pool <- Database.mkConnectionIO @Postgres conf
    Pool.withResource pool $ \conn -> do
      let defaultContext =
            MigrationContext
              { migrationContextCommand = MigrationInitialization,
                migrationContextVerbose = False,
                migrationContextConnection = conn
              }
          migrations =
            ("(init)", defaultContext) :
              [ ( k,
                  defaultContext
                    { migrationContextCommand =
                        MigrationScript k v
                    }
                )
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

  postToDatabase pc q a = pgHandler $
    Pool.withResource pc $ \conn ->
      formatQuery conn q a >>= B.putStr
        >> query conn q a >>= Database.getSingle

  getFromDatabase pc q a = pgHandler $
    Pool.withResource pc $ \conn ->
      formatQuery conn q a >>= B.putStr
        >> query conn q a

  putIntoDatabase pc q a = pgHandler $
    fmap fromIntegral $
      Pool.withResource pc $ \conn ->
        formatQuery conn q a >>= B.putStr
          >> execute conn q a

  deleteFromDatabase pc q a = pgHandler $
    fmap fromIntegral $
      Pool.withResource pc $ \conn ->
        formatQuery conn q a >>= B.putStr
          >> execute conn q a

sortedMigrations :: [(FilePath, B.ByteString)]
sortedMigrations =
  let unsorted = $(embedDir "migrations")
   in L.sortBy (compare `on` fst) unsorted

pgHandler :: IO a -> IO a
pgHandler =
  flip
    catches
    [ Handler handleSqlErrror,
      Handler handleFormatError
    ]

handleSqlErrror :: SqlError -> IO a
handleSqlErrror SqlError {..} = throwM $ case sqlState of
  "23503" -> Database.EntityNotFound $ T.decodeUtf8 sqlErrorDetail
  "23502" -> Database.IsNull $ T.decodeUtf8 sqlErrorDetail
  "23505" -> Database.AlreadyExists $ T.decodeUtf8 sqlErrorDetail
  "23514" -> Database.OtherError $ T.decodeUtf8 sqlErrorMsg
  _ -> Database.UnknwonError $ T.show SqlError {..}

handleFormatError :: FormatError -> IO a
handleFormatError f = throwM $ Database.UnknwonError $ T.show f

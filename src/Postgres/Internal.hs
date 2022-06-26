{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Postgres.Internal where

import Control.Monad (forM_)
import Control.Monad.Catch
  ( Handler (Handler),
    MonadCatch,
    MonadThrow (..),
    catches,
  )
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString qualified as B hiding (putStrLn)
import Data.FileEmbed (embedDir)
import Data.Function (on)
import Data.List qualified as L
import Data.Pool qualified as Pool
import Database.Config qualified as Database
import Database.HasDatabase qualified as Database
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
  ( Connection,
    ConnectInfo (..),
    FormatError,
    FromRow,
    SqlError (..),
    ToRow,
    close,
    connect,
    execute,
    formatQuery,
    query, 
  )
import Extended.Text qualified as T
import Logger qualified
import System.Exit (exitFailure)

type PGConnection = Pool.Pool Connection

type PGFromRow = FromRow

type PGToRow = ToRow

type HasPGDatabase m =
  ( Monad m,
    MonadIO m,
    MonadCatch m,
    Logger.HasLogger m,
    Database.HasDatabase m,
    Database.ConnectionOf m ~ PGConnection
  )

queryWithlog ::
  (ToRow a, FromRow r, HasPGDatabase m) =>
  Database.DBQuery ->
  a ->
  m [r]
queryWithlog (Database.fromQuery -> q) a = pgHandler $ do
  Logger.sql $ T.show q
  pc <- Database.getDatabaseConnection
  (queryToLog, res) <- liftIO $
    Pool.withResource pc $ \conn -> do
      queryToLog <- liftIO $ T.decodeUtf8 <$> formatQuery conn q a
      res <- liftIO $ query conn q a
      pure (queryToLog, res)
  Logger.sql queryToLog
  pure res

executeWithLog ::
  (ToRow a, HasPGDatabase m) =>
  Database.DBQuery ->
  a ->
  m Integer
executeWithLog (Database.fromQuery -> q) a = pgHandler $ do
  Logger.sql $ T.show q
  pc <- Database.getDatabaseConnection
  (queryToLog, res) <- liftIO $
    Pool.withResource pc $ \conn -> do
      queryToLog <- liftIO $ T.decodeUtf8 <$> formatQuery conn q a
      res <- liftIO $ fromIntegral <$> execute conn q a
      pure (queryToLog, res)
  Logger.sql queryToLog
  pure res

mkConnectInfo :: Database.Config -> ConnectInfo
mkConnectInfo Database.Config {..} = ConnectInfo
  { connectHost = T.unpack cHost
  , connectPort = fromIntegral cPort
  , connectUser = T.unpack cUser
  , connectPassword = T.unpack cPassword
  , connectDatabase = T.unpack cDatabase
  }

mkConnectionIO :: Database.Config -> IO (Pool.Pool Connection)
mkConnectionIO c = 
  Pool.createPool
    (connect $ mkConnectInfo c)
    close
    1
    30
    4

pgHandler :: (MonadCatch m, Database.HasDatabase m) => m a -> m a
pgHandler ma =
  Database.handleDBErrors $
    catches
      ma
      [ Handler handleSqlErrror,
        Handler handleFormatError
      ]

handleSqlErrror :: MonadThrow m => SqlError -> m a
handleSqlErrror SqlError {..} = throwM $ case sqlState of
  "23503" -> Database.EntityNotFound $ T.decodeUtf8 sqlErrorDetail
  "23502" -> Database.IsNull $ T.decodeUtf8 sqlErrorDetail
  "23505" -> Database.AlreadyExists $ T.decodeUtf8 sqlErrorDetail
  "23514" -> Database.ConstraintViolation $ T.decodeUtf8 sqlErrorMsg
  _ -> Database.UnknwonError $ T.show SqlError {..}

handleFormatError :: MonadThrow m => FormatError -> m a
handleFormatError f = throwM $ Database.FormatError $ T.show f

runMigrations :: Database.Config -> Logger.Logger IO -> IO ()
runMigrations conf l = do
  pool <- mkConnectionIO conf
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

sortedMigrations :: [(FilePath, B.ByteString)]
sortedMigrations =
  let unsorted = $(embedDir "migrations")
   in L.sortBy (compare `on` fst) unsorted

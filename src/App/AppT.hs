{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}

module App.AppT where

import App.Config (Config (..))
import App.Error (handleDBErrors)
import App.Impure (Impure)
import App.Path (Current, Path)
import App.QueryParams (QueryParams)
import App.Types (Body, ContentType, Token)
import Control.Monad.Catch (MonadCatch, MonadThrow (..))
import Control.Monad.Reader
  ( MonadIO (..),
    MonadReader,
    MonadTrans (..),
    ReaderT (..),
    asks,
    (>=>),
  )
import Data.Kind (Type)
import Database.HasDatabase qualified as Database
import Database.Internal qualified as Database
import Logger (HasLogger)
import Logger qualified
import Postgres.Internal qualified as Postgres

type Application (m :: Type -> Type) =
  ( MonadThrow m,
    MonadCatch m,
    HasEnv m,
    Impure m,
    Logger.HasLogger m,
    Database.HasDatabase m
  )

newtype AppT m a = App {unApp :: ReaderT (Env m) m a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader (Env m)
    )

runApp ::
  (Application (AppT m)) =>
  Env m ->
  AppT m a ->
  m a
runApp env app = runReaderT (unApp $ handleDBErrors app) env

deriving newtype instance MonadThrow m => MonadThrow (AppT m)

deriving newtype instance MonadIO (AppT IO)

deriving newtype instance MonadCatch (AppT IO)

instance MonadTrans AppT where
  lift ma = App $ lift ma

deriving anyclass instance Impure (AppT IO)

instance Logger.HasLogger (AppT IO) where
  mkLog v t = asks envLogger >>= \l -> lift (l v t)

instance Database.HasDatabase (AppT IO) where
  type ConnectionOf (AppT IO) = Postgres.PGConnection
  getDatabaseConnection = asks envConn
  type FromRowOf (AppT IO) q = Postgres.PGFromRow q
  type ToRowOf (AppT IO) q = Postgres.PGToRow q
  runMigrations = Postgres.runMigrations
  mkConnectionIO = Postgres.mkConnectionIO
  postToDatabase q = Postgres.queryWithlog q >=> Database.getSingle
  getFromDatabase = Postgres.queryWithlog
  putIntoDatabase = Postgres.executeWithLog
  deleteFromDatabase = Postgres.executeWithLog
  handleDBErrors = handleDBErrors

data Env (m :: Type -> Type) = Env
  { envLogger :: !(Logger.Logger m),
    envConn :: !(Database.ConnectionOf (AppT m)),
    envPath :: !(Path Current),
    envBody :: !Body,
    envContentType :: !(Maybe ContentType),
    envQParams :: !QueryParams,
    envToken :: !(Maybe Token),
    envConfig :: !Config
  }

type family EnvOf a where
  EnvOf (AppT m) = m

type HasEnv m = MonadReader (Env (EnvOf m)) m

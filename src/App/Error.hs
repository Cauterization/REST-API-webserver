{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ViewPatterns #-}

module App.Error where

import App.Path (Path, Pattern)
import Control.Monad.Catch
  ( Exception,
    MonadCatch,
    MonadThrow (..),
    handle,
  )
import Data.Typeable (Typeable)
import Database.Internal qualified as Database
import Extended.Text (Text)
import Extended.Text qualified as T
import Logger qualified
import Network.HTTP.Types qualified as HTTP

data AppError
  = PageNotFoundError
  | ParsingError !Text
  | UnknwonHTTPMethod !HTTP.Method
  | QParamsError
  | RequestHeadersError !Text
  | RouterAmbiguousPatterns ![Path Pattern]
  | Unathorized !Text
  | AccessViolation !Text
  | AdminAccessViolation
  | WrongPassword
  | EntityIDArityMissmatch !Text
  | EntityNotFound !Text
  | TooManyEntities !Text
  | AlreadyExists !Text
  | CategoryCycle
  | IsNull !Text
  | ConstraintViolation !Text
  | DatabaseFormatError !Text
  | UnknownError !Text
  deriving (Show, Typeable, Exception, Eq)

handleDBErrors :: (MonadCatch m, Logger.HasLogger m) => m a -> m a
handleDBErrors = handle $ \err -> (>> throwM (fromDBError err)) $ case err of
  Database.EntityNotFound t -> Logger.info t
  Database.TooManyEntities t -> Logger.error t
  Database.AlreadyExists t -> Logger.info t
  Database.IsNull t -> Logger.info t
  Database.ConstraintViolation t -> Logger.info t
  Database.FormatError t -> Logger.error t
  Database.UnknwonError t -> Logger.error t

fromDBError :: Database.DBError -> AppError
fromDBError = \case
  Database.EntityNotFound t -> EntityNotFound t
  Database.TooManyEntities t -> TooManyEntities t
  Database.AlreadyExists t -> AlreadyExists t
  Database.IsNull t -> IsNull t
  Database.ConstraintViolation t -> DatabaseFormatError t
  Database.FormatError t -> DatabaseFormatError t
  Database.UnknwonError t -> UnknownError t

parsingError :: (MonadThrow m, Logger.HasLogger m) => String -> m a
parsingError (T.pack -> err) = do
  Logger.info $ "Unable to parse: " <> err
  throwM $ ParsingError err

ambiguousPatterns :: (MonadThrow m, Logger.HasLogger m) => [Path Pattern] -> m a
ambiguousPatterns ps = do
  Logger.error $ "Ambiguous patterns in router pathes: " <> T.show ps
  throwM $ RouterAmbiguousPatterns ps

unathorizedError,
  idArityMissmatchError,
  accessViolationError,
  requestHeadersError,
  queryParamsError,
  wrongPasswordError ::
    (MonadThrow m, Logger.HasLogger m) => Text -> m a
unathorizedError err = do
  Logger.info err
  throwM $ Unathorized err
idArityMissmatchError err = do
  Logger.error $ "Entity ID arity missmatch: " <> err
  throwM $ EntityIDArityMissmatch err
accessViolationError err = do
  Logger.warning $ "Access violation: " <> err
  throwM $ AccessViolation err
requestHeadersError err = do
  Logger.info err
  throwM $ RequestHeadersError err
wrongPasswordError uLogin = do
  Logger.info $ "Wrong password for user: " <> uLogin
  throwM WrongPassword
queryParamsError err = do
  Logger.info $ "Query parameters error: " <> err
  throwM QParamsError

adminAccessViolationError, categoryCycleError, pageNotFoundError :: (MonadThrow m, Logger.HasLogger m) => m a
adminAccessViolationError = do
  Logger.warning "Admin access violation!"
  throwM AdminAccessViolation
categoryCycleError = do
  Logger.warning "Unacceptable category update - cycle in category tree!"
  throwM CategoryCycle
pageNotFoundError = do
  Logger.info "Page doesn't exists."
  throwM PageNotFoundError

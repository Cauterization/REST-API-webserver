{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App.Internal where

import App.Config (Config (..))
import App.QueryParams (QueryParams)
import App.Types
  ( Body,
    ContentType,
    Current,
    Date,
    Path (..),
    Pattern,
    Token,
  )
import Control.Monad.Catch
  ( Exception,
    MonadCatch,
    MonadThrow (..),
    handle,
  )
import Control.Monad.Reader
  ( MonadIO (..),
    MonadReader,
    MonadTrans (..),
    ReaderT (..),
    asks,
    replicateM,
  )
import Data.Aeson (FromJSON, eitherDecode)
import Data.Char (toLower)
import Data.Data (Typeable)
import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.Map qualified as M
import Data.Time qualified as Time
import Database.Database qualified as Database
import Extended.Text (Text)
import Extended.Text qualified as T
import Logger (HasLogger, (.<))
import Logger qualified
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Postgres.Internal (Postgres)
import System.Random (randomRIO)

type Application (m :: Type -> Type) =
  ( MonadThrow m,
    MonadCatch m,
    HasEnv m,
    Impure m,
    Logger.HasLogger m,
    Database.HasDatabase m,
    Database.QConstraints (Database.Database m)
  )

newtype AppT m a = App {unApp :: ReaderT (Env m) m a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader (Env m)
    )

runApp ::
  (Monad m, MonadCatch (AppT m)) =>
  Env m ->
  AppT m a ->
  m a
runApp env app = runReaderT (unApp app) env

type DB m = Database.Database (AppT m)

deriving newtype instance MonadThrow m => MonadThrow (AppT m)

deriving newtype instance MonadIO (AppT IO)

deriving newtype instance MonadCatch (AppT IO)

deriving anyclass instance MonadIO (AppT m) => Logger.RunLogger (AppT m)

instance MonadTrans AppT where
  lift = App . lift

instance Monad m => Logger.HasLogger (AppT m) where
  mkLog v t = asks envLogger >>= \l -> lift $ l v t

instance Database.HasDatabase (AppT IO) where
  type Database (AppT IO) = Postgres

  liftDatabase = handle (throwM . fromDBException) . liftIO

  getDatabaseConnection = asks envConn

class Impure m where
  getCurrentDate :: m Date
  genToken :: m Token

instance Impure (AppT IO) where
  getCurrentDate = liftIO $ Time.getCurrentTime <&> Time.utctDay
  genToken =
    let chars = ['0' .. '9'] ++ ['A' .. 'Z'] ++ ['a' .. 'z']
     in fmap T.pack $
          replicateM 16 $ do
            idx <- randomRIO (0, length chars - 1)
            return $ chars !! idx

data Env (m :: Type -> Type) = Env
  { envLogger :: !(Logger.Logger m),
    envConn :: !(Database.ConnectionOf (Database.Database (AppT m))),
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

decodedBody ::
  ( FromJSON a,
    Monad m,
    MonadThrow m,
    HasEnv m,
    Logger.HasLogger m
  ) =>
  m a
decodedBody = do
  body <- asks envBody
  Logger.debug $ T.take 500 $ "JSON body:\n" .< body
  decode body

decode :: (Monad m, FromJSON a, MonadThrow m, HasLogger m) => Body -> m a
decode = either parsingError pure . eitherDecode

getParam :: (HasEnv m, Logger.HasLogger m, MonadThrow m) => Text -> m (Maybe Text)
getParam (T.map toLower -> p) =
  asks (M.lookup p . envQParams) >>= \case
    Just [a] -> pure $ Just a
    Nothing -> pure Nothing
    _ -> queryParamsError $ "multiparam " <> p

getNumParam ::
  ( Monad m,
    MonadThrow m,
    Logger.HasLogger m,
    HasEnv m
  ) =>
  Text ->
  m (Maybe Int)
getNumParam p =
  fmap T.read <$> getParam p >>= \case
    Just (Right n) -> pure $ Just n
    Nothing -> pure Nothing
    Just (Left _) -> parsingError $ "Unparsable num param " <> (T.unpack p)

getNumListParam ::
  ( Monad m,
    MonadThrow m,
    Logger.HasLogger m,
    HasEnv m
  ) =>
  Text ->
  m (Maybe [Int])
getNumListParam p =
  fmap T.read <$> getParam p >>= \case
    Just (Right n) -> pure $ Just n
    Nothing -> pure Nothing
    Just (Left _) -> parsingError $ "Unparsable num list param " <> (T.unpack p)

getDateParam ::
  ( Monad m,
    MonadThrow m,
    Logger.HasLogger m,
    HasEnv m
  ) =>
  Text ->
  m (Maybe Date)
getDateParam p =
  fmap (parseTime . T.unpack) <$> getParam p >>= \case
    Just (Just d) -> pure $ Just d
    Nothing -> pure Nothing
    Just Nothing -> parsingError $ "Unparsable date param " <> (T.unpack p)
  where
    parseTime = Time.parseTimeM True Time.defaultTimeLocale "%Y-%m-%d"

getToken :: (Monad m, HasEnv m, MonadThrow m, HasLogger m) => m Token
getToken = asks envToken >>= maybe (unathorizedError "No token.") pure

getServerAddress :: HasEnv m => m Text
getServerAddress = do
  Config {..} <- asks envConfig
  pure $ cAddress <> T.show cPort

toPath :: Wai.Request -> Path a
toPath req =
  let path = Wai.pathInfo req
   in case Wai.requestMethod req of
        "POST" -> POST path
        "GET" -> GET path
        "PUT" -> PUT path
        "DELETE" -> DELETE path
        "PUBLISH" -> PUBLISH path
        _ -> Unknown path

data AppError
  = PageNotFoundError (Path Current)
  | ParsingError Text
  | UnknwonHTTPMethod HTTP.Method
  | QParamsError
  | RequestHeadersError Text
  | RouterAmbiguousPatterns [Path Pattern]
  | Unathorized Text
  | AccessViolation Text
  | AdminAccessViolation
  | WrongPassword
  | EntityIDArityMissmatch Text
  | EntityNotFound Text
  | TooManyEntities Text
  | AlreadyExists Text
  | CategoryCycle
  | IsNull Text
  | DatabaseOtherError Text
  | UnknownError Text
  deriving (Show, Typeable, Exception, Eq)

fromDBException :: Database.DBError -> AppError
fromDBException = \case
  Database.EntityNotFound t -> EntityNotFound t
  Database.TooManyEntities t -> TooManyEntities t
  Database.AlreadyExists t -> AlreadyExists t
  Database.IsNull t -> IsNull t
  Database.OtherError t -> DatabaseOtherError t
  Database.UnknwonError t -> UnknownError t

parsingError :: (MonadThrow m, HasLogger m) => String -> m a
parsingError (T.pack -> err) = do
  Logger.info $ "Unable to parse: " <> err
  throwM $ ParsingError err

pageNotFoundError :: (MonadThrow m, HasEnv m, HasLogger m) => m a
pageNotFoundError = do
  p <- asks envPath
  Logger.info $ "Page doesn't exists: " <> T.show p
  throwM $ PageNotFoundError p

ambiguousPatterns :: (MonadThrow m, HasLogger m) => [Path Pattern] -> m a
ambiguousPatterns ps = do
  Logger.error $ "Ambiguous pattern in router pathes: " <> T.show ps
  throwM $ RouterAmbiguousPatterns ps

unathorizedError,
  idArityMissmatchError,
  accessViolationError,
  requestHeadersError,
  queryParamsError,
  wrongPasswordError ::
    (MonadThrow m, HasLogger m) => Text -> m a
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
  Logger.info $ "Entity ID arity missmatch: " <> err
  throwM $ RequestHeadersError err
wrongPasswordError uLogin = do
  Logger.info $ "Wrong password for user: " <> uLogin
  throwM WrongPassword
queryParamsError err = do
  Logger.info $ "Query parameters error: " <> err
  throwM QParamsError

adminAccessViolationError, categoryCycleError :: (MonadThrow m, HasLogger m) => m a
adminAccessViolationError = do
  Logger.warning "Admin access violation!"
  throwM AdminAccessViolation
categoryCycleError = do
  Logger.warning "Unacceptable category update - cycle in category tree!"
  throwM CategoryCycle

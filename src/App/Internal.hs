{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}

module App.Internal where

import Control.Monad.Catch 
import Control.Monad.Reader 

import Data.Aeson (FromJSON, eitherDecode)
import Data.Char (toLower)
import Data.Kind (Type)
import Extended.Text (Text)
import Extended.Text qualified as T
import App.QueryParams
import App.Config

import App.Types
import System.Random
import Logger ((.<))
import Logger qualified 
import qualified Network.HTTP.Types as HTTP
import qualified Data.Map as M
import Control.Arrow ((>>>))
import Data.Maybe
import Data.Functor
import Data.Data (Typeable)
import qualified Network.Wai as Wai

import Database.Database qualified as Database

import Postgres.Internal
import qualified Data.Time as Time

type Application (m :: Type -> Type) = 
    ( MonadThrow m
    , MonadCatch m
    , HasEnv m
    , Impure m
    , Logger.HasLogger m
    , Database.HasDatabase m
    , Database.QConstraints (Database.Database m)
    )

newtype AppT m a = App {unApp :: ReaderT (Env m) m a}
    deriving newtype 
        ( Functor
        , Applicative
        , Monad
        , MonadReader (Env m)
        )

runApp :: (Monad m, MonadCatch (AppT m)) 
    => Env m -> AppT m a -> m a
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
    genToken       :: m Token

instance Impure (AppT IO) where
    getCurrentDate = liftIO $ Time.getCurrentTime <&> Time.utctDay
    genToken = let chars = ['0' .. '9'] ++ ['A' .. 'Z'] ++ ['a' .. 'z']  
        in fmap T.pack $ replicateM 16 $ do
            idx <- randomRIO (0, length chars - 1)
            return $ chars !! idx

data Env (m :: Type -> Type) = Env
    { envLogger      :: Logger.Logger m
    , envConn        :: Database.ConnectionOf (Database.Database (AppT m))
    , envPath        :: Path Current
    , envBody        :: Body
    , envContentType :: Maybe ContentType 
    , envQParams     :: QueryParams
    , envToken       :: Maybe Token
    , envConfig      :: Config
    }
   
type family EnvOf a where
    EnvOf (AppT m) = m 

type HasEnv m = MonadReader (Env (EnvOf m)) m

decodedBody :: (FromJSON a, Monad m, MonadThrow m, HasEnv m, Logger.HasLogger m
    ) => m a
decodedBody = do
    body <- asks envBody
    Logger.debug $ T.take 500 $ "JSON body:\n" .< body
    decode body

decode :: (Monad m, FromJSON a, MonadThrow m) => Body -> m a
decode = either parsingError pure . eitherDecode  

getParam :: (HasEnv m, MonadThrow m) => Text -> m (Maybe Text)
getParam (T.map toLower -> p) = asks (M.lookup p . envQParams) >>= \case
    Just [a] -> pure $ Just a
    Nothing -> pure Nothing
    _ -> throwM $ QParamsErr $ "multiparam " <> p 

getNumParam :: (Monad m, MonadThrow m, Logger.HasLogger m, HasEnv m
    ) => Text -> m (Maybe Int)
getNumParam p = fmap T.read <$> getParam p >>= \case
    Just (Right n) -> pure $ Just n
    Nothing        -> pure Nothing
    Just (Left _)  -> parsingError $ "Unparsable num param " <> (T.unpack p)

getDateParam :: (Monad m, MonadThrow m, Logger.HasLogger m, HasEnv m
    ) => Text -> m (Maybe Date)
getDateParam p = fmap (parseTime . T.unpack) <$> getParam p >>= \case
    Just (Just d) -> pure $ Just d
    Nothing -> pure Nothing
    Just Nothing ->  parsingError $ "Unparsable date param " <> (T.unpack p)
  where
    parseTime = Time.parseTimeM True Time.defaultTimeLocale "%Y-%m-%d"

getToken :: (Monad m, HasEnv m, MonadThrow m) =>  m Token
getToken = asks envToken >>= maybe (unathorized "No token.") pure

getServerAddress :: HasEnv m => m Text
getServerAddress = do
    Config{..} <- asks (envConfig)
    pure $ cAddress <> T.show cPort 

toPath ::  Wai.Request -> Path a
toPath req = 
    let path = Wai.pathInfo req
    in case Wai.requestMethod req of
        "POST"    -> POST    path
        "GET"     -> GET     path
        "PUT"     -> PUT     path
        "DELETE"  -> DELETE  path
        "PUBLISH" -> PUBLISH path
        _         -> Unknown path

data AppError 
    = Err404 (Path Current)
    | ParsingErr Text
    | UnknwonHTTPMethod HTTP.Method
    | QParamsErr Text
    | RequestHeadersErr Text
    | RouterAmbiguousPatterns [Path Pattern]
    | Unathorized Text
    | AccessViolation Text
    | AdminAccessViolation Text
    | WrongPassword
    | EntityIDArityMissmatch Text
    | EntityNotFound Text
    | TooManyEntities Text
    | AlreadyExists Text
    | CategoryCycle
    | IsNull Text
    | DatabaseOtherError Text
    | UnknownError  Text
    deriving (Show, Typeable, Exception, Eq) 

fromDBException :: Database.DBError -> AppError
fromDBException = \case
    Database.EntityNotFound  t -> EntityNotFound  t
    Database.TooManyEntities t -> TooManyEntities t
    Database.AlreadyExists   t -> AlreadyExists   t
    Database.IsNull          t -> IsNull t
    Database.OtherError      t -> DatabaseOtherError t
    Database.UnknwonError    t -> UnknownError t

parsingError :: (MonadThrow m) => String -> m a
parsingError = throwM . ParsingErr . T.pack 

throw404 :: (MonadThrow m, HasEnv m) =>  m a
throw404 = asks envPath >>= throwM . Err404 

ambiguousPatterns :: (MonadThrow m) => [Path Pattern] -> m a
ambiguousPatterns = throwM . RouterAmbiguousPatterns

unathorized  :: (MonadThrow m) => Text -> m a
unathorized = throwM . Unathorized 

entityIDArityMissmatch :: MonadThrow m => Text -> m a
entityIDArityMissmatch = throwM . EntityIDArityMissmatch
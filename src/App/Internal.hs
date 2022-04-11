{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App.Internal where

import Control.Monad.Catch 
import Control.Monad.Reader 

import Data.Aeson (FromJSON, eitherDecode)
import Data.Kind (Type)
import Extended.Text (Text)
import Extended.Text qualified as T
import App.QueryParams

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

newtype AppT m a = App {unApp :: ReaderT (Env m) m a}
    deriving newtype 
        ( Functor
        , Applicative
        , Monad
        , MonadReader (Env m)
        )

runApp :: (Monad m, MonadCatch (AppT m)) 
    => Env m -> AppT m a -> m (Either AppError a)
runApp env app = runReaderT (unApp $ try app) env

type DB m = Database.Database (AppT m)

deriving newtype  instance MonadThrow m => MonadThrow (AppT m)
deriving newtype  instance MonadIO (AppT IO)
deriving newtype  instance MonadCatch (AppT IO)
deriving anyclass instance MonadIO (AppT m) => Logger.RunLogger (AppT m)

instance MonadTrans AppT where
    lift = App . lift

instance Monad m => Logger.HasLogger (AppT m) where
    mkLog v t = asks envLogger >>= \l -> lift $ l v t 

instance Database.HasDatabase (AppT IO) where

    type Database (AppT IO) = Postgres

    liftDatabase = liftIO

    getDatabaseConnection = asks envConn

    getPaginationSize = asks envPagination

class Impure m where
    getCurrentDate :: m Date 
    genToken :: m Token

instance Impure (AppT IO) where
    getCurrentDate = liftIO $ Time.getCurrentTime <&> Time.utctDay
    genToken = let chars = ['0' .. '9'] ++ ['A' .. 'Z'] ++ ['a' .. 'z']  
        in fmap T.pack $ replicateM 16 $ do
            idx <- randomRIO (0, length chars - 1)
            return $ chars !! idx

instance {-# OVERLAPPABLE #-} Monad m => Impure (AppT m) where
    getCurrentDate = pure $ Time.fromGregorian 1 2 3
    genToken = pure "super unique token"

data Env (m :: Type -> Type) = Env
    { envLogger     :: Logger.Logger m
    , envConn       :: Database.ConnectionOf (Database.Database (AppT m))
    , envPath       :: Path Current
    , envBody       :: Body
    , envQParams    :: QueryParams
    , envToken      :: Maybe Token
    , envPagination :: PaginationSize
    }
   
type family EnvOf a where
    EnvOf (AppT m) = m 

type HasEnv m = MonadReader (Env (EnvOf m)) m

getPath :: HasEnv m => m (Path Current)
getPath = asks envPath

decodedBody :: (FromJSON a, Monad m, MonadThrow m, HasEnv m, Logger.HasLogger m
    ) => m a
decodedBody = do
    body <- asks envBody
    Logger.debug $ "JSON body:\n" .< body
    decode body

decode :: (Monad m, FromJSON a, MonadThrow m) => Body -> m a
decode = either parsingError pure . eitherDecode  

getParam :: (HasEnv m, MonadThrow m) => Text -> m (Maybe Text)
getParam p = asks (M.lookup p . envQParams) >>= \case
    Just [a] -> pure $ Just a
    Nothing -> pure Nothing
    _ -> throwM $ QParamsErr $ "multiparam " <> p 

getPage :: (Monad m, MonadThrow m, Logger.HasLogger m, HasEnv m)  => m Page
getPage = getParam "page" <&> (fromMaybe "1" >>> T.read) >>= either
    (parsingError . ("page: " <>) .  T.unpack)
    (\n -> if n > 0 then pure n else throwM $ QParamsErr "zero or negative page")

getToken :: (Monad m, HasEnv m, MonadThrow m) =>  m Token
getToken = asks envToken >>= maybe (unathorized "No token.") pure

toPath ::  Wai.Request -> Path a
toPath req = 
    let path = Wai.pathInfo req 
    in case Wai.requestMethod req of
        "POST"   -> POST    path
        "GET"    -> GET     path
        "PUT"    -> PUT     path
        "DELETE" -> DELETE  path
        _        -> Unknown path

data AppError 
    = Err404 (Path Current)
    | ParsingErr Text
    | UnknwonHTTPMethod HTTP.Method
    | QParamsErr Text
    | RouterAmbiguousPatterns [Path Pattern]
    | Unathorized Text
    | AccessViolation Text
    | WrongPassword
    | EntityIDArityMissmatch Text
    | EntityNotFound Text
    | TooManyEntities Text
    | AlreadyExists Text
    deriving (Show, Typeable, Exception, Eq) 

fromDBException :: Database.DBError -> AppError
fromDBException = \case
    Database.EntityNotFound  t -> EntityNotFound  t
    Database.TooManyEntities t -> TooManyEntities t
    Database.AlreadyExists   t -> AlreadyExists   t

parsingError :: (MonadThrow m) => String -> m a
parsingError = throwM . ParsingErr . T.pack 

throw404 :: (MonadThrow m, HasEnv m) =>  m a
throw404 = getPath >>= throwM . Err404 

ambiguousPatterns :: (MonadThrow m) => [Path Pattern] -> m a
ambiguousPatterns = throwM . RouterAmbiguousPatterns

unathorized  :: (MonadThrow m) => Text -> m a
unathorized = throwM . Unathorized 

entityIDArityMissmatch :: MonadThrow m => Text -> m a
entityIDArityMissmatch = throwM . EntityIDArityMissmatch
{-# LANGUAGE DeriveDataTypeable #-}

module Server.Base where



import Data.Text (Text)
import qualified Data.ByteString.Lazy as BL
import GHC.Generics
import Data.Data
import Control.Monad.Catch
import qualified Network.Wai as Wai

import Network.HTTP.Types qualified as HTTP
import Data.Map qualified as M
import qualified Data.Text.Encoding as T
import Data.Function (on)
import Data.Maybe

import Logger.Handle qualified as Logger
import Database.Config qualified as Database
import Data.Aeson
import qualified Extended.Text as T
import Control.Arrow ((>>>))
import Data.Functor

import Types

data AppResult 
    = ResText Text 
    | ResJSON BL.ByteString
    | ResPicture
    deriving Show 

text :: Applicative m => Text -> m AppResult
text = pure . ResText

json :: (Applicative m, ToJSON e) => e -> m AppResult
json = pure . ResJSON . encode

type Endpoint m = [ID Path] -> m AppResult

data AppErr 
    = Err404 (Path Current)
    | ParsingErr Text
    | UnknwonHTTPMethod HTTP.Method
    | QParamsErr Text
    | OtherErr Text
    deriving (Show, Typeable, Exception) 

fromErr :: AppErr -> AppResult
fromErr = ResText . T.show

throwWithInfo :: (MonadThrow m, Logger.HasLogger m) => AppErr -> m a
throwWithInfo e = do
    Logger.info $ T.show e
    throwM e

throwParsingError :: (MonadThrow m, Logger.HasLogger m) => String -> m a
throwParsingError = throwWithInfo . ParsingErr . T.pack 

throw404 :: (MonadThrow m, HasEnv m, Logger.HasLogger m) =>  m a
throw404 = getPath >>= throwWithInfo . Err404 

data Config = Config
    { cDatabase :: Database.Config
    , cLogger   :: Logger.Config
    } deriving (Show, Generic, FromJSON)

type QParams = M.Map Text [Text]

toQParams :: HTTP.Query -> QParams
toQParams = M.unionsWith (++) . fmap ( M.map pure 
                                     . uncurry (M.singleton `on` T.decodeUtf8) 
                                     . fmap (fromMaybe ""))

toPath :: (Monad m, MonadThrow m) => Wai.Request -> m (Path a)
toPath req = 
    let path = Wai.pathInfo req 
    in case Wai.requestMethod req of
        "POST"   -> pure $ POST   path
        "GET"    -> pure $ GET    path
        "PUT"    -> pure $ PUT    path
        "DELETE" -> pure $ DELETE path
        unknown  -> throwM $ UnknwonHTTPMethod unknown

data Env = Env
    { envPath    :: Path Current
    , envBody    :: Body
    , envConfig  :: Config
    , envQParams :: QParams
    }

toEnv :: (Monad m, MonadThrow m) 
    => Config 
    -> Wai.Request 
    -> Body 
    -> m Env
toEnv envConfig req envBody = do
    envPath <- toPath req
    let envQParams = toQParams $ Wai.queryString req
    pure Env{..}

class HasEnv m where
    asksEnv :: (Env -> a) -> m a

getPath :: HasEnv m => m (Path Current)
getPath = asksEnv envPath

getBody :: HasEnv m => m Body
getBody = asksEnv envBody

getConfig :: HasEnv m => (Config -> a) -> m a
getConfig f = asksEnv (f . envConfig)

getParam :: (HasEnv m, MonadThrow m) => Text -> m (Maybe Text)
getParam p = asksEnv (M.lookup p . envQParams) >>= \case
    Just [a] -> pure $ Just a
    Nothing -> pure Nothing
    _ -> throwM $ QParamsErr $ "multiparam " <> p 

decodedBody :: (FromJSON e, Monad m, Logger.HasLogger m, MonadThrow m, HasEnv m) 
    => m e 
decodedBody = getBody >>= either throwParsingError pure . eitherDecode

getPage :: (Monad m, MonadThrow m, Logger.HasLogger m, HasEnv m)  => m Page
getPage = getParam "page" <&> (fromMaybe "1" >>> T.read) >>= \case
    Left err -> throwWithInfo $ ParsingErr $ "page: " <> err
    Right n  -> if n > 0 
                then pure n
                else throwWithInfo $ QParamsErr "zero or negative page"
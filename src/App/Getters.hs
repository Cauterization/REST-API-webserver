module App.Getters where

import App.AppT
  ( Env (envBody, envConfig, envQParams, envToken),
    HasEnv,
  )
import App.Config
  ( Config (Config, cAddress, cDatabase, cLogger, cPort),
  )
import App.Error
  ( parsingError,
    queryParamsError,
    unathorizedError,
  )
import App.Types (Body, Date, Token)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Reader (asks)
import Data.Aeson (FromJSON, eitherDecode)
import Data.Char (toLower)
import Data.Map qualified as M
import Data.Time qualified as Time
import Extended.Text (Text)
import Extended.Text qualified as T
import Logger ((.<))
import Logger qualified

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
  decodeBody body

decodeBody :: (Monad m, FromJSON a, MonadThrow m, Logger.HasLogger m) => Body -> m a
decodeBody = either parsingError pure . eitherDecode

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

getToken :: (Monad m, HasEnv m, MonadThrow m, Logger.HasLogger m) => m Token
getToken = asks envToken >>= maybe (unathorizedError "No token.") pure

getServerAddress :: HasEnv m => m Text
getServerAddress = do
  Config {..} <- asks envConfig
  pure $ cAddress <> T.show cPort

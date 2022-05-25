{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module App.RunServer where

import App.AppT (AppT)
import App.Config (Config (..))
import App.Endpoints (Main)
import App.Error (AppError (..))
import App.Path (toPath)
import App.QueryParams (toQueryParams)
import App.Result (AppResult (..))
import App.Router (runRouter)
import App.Types (Body)
import Control.Exception (IOException)
import Control.Monad.Catch (handle)
import Control.Monad.Extra (whenM)
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy qualified as BL
import Data.Char (toLower)
import Data.String (fromString)
import Database.HasDatabase qualified as Database
import Entity.Picture (Picture (Picture))
import Extended.Text qualified as T
import Logger ((.<))
import Logger qualified
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Wai
import System.Environment (getArgs)
import System.Exit qualified as Sys

runServer :: IO ()
runServer = handle handler $ do
  Config {..} <- BL.readFile "config.json" >>= parseOrFail
  connectionDB <- Database.mkConnectionIO @(AppT IO) cDatabase
  let logger = Logger.fromConfig cLogger
  whenM (("migrations" `elem`) <$> getArgs) $
    Database.runMigrations @(AppT IO) cDatabase logger
  processRequest logger Config {..} connectionDB
  where
    handler (e :: IOException) = Sys.die $ show e <> ". Terminating..."
    parsingFail = fail . ("Parsing config error: " <>) . show
    parseOrFail = either parsingFail pure . eitherDecode

processRequest ::
  Logger.Logger IO ->
  Config ->
  Database.ConnectionOf (AppT IO) ->
  IO ()
processRequest logger Config {..} connectionDB = Wai.run cPort $ \req respond -> do
  body <- Wai.strictRequestBody req
  logger Logger.Debug $
    T.take 1000 $ "Recieved request:\n" .< req
  ToResponse {..} <-
    let getFromHeaders p = T.decodeUtf8 <$> lookup p (Wai.requestHeaders req)
     in either responseFromError responseFromResult
          <$> runRouter @Main
            logger
            connectionDB
            (toPath req)
            body
            (getFromHeaders "Content-Type")
            (toQueryParams $ Wai.queryString req)
            (getFromHeaders "Authorization")
            Config {..}
  respond $ Wai.responseLBS respStatus respHeaders respBody

data ToResponse = ToResponse
  { respStatus :: !HTTP.Status,
    respHeaders :: ![HTTP.Header],
    respBody :: !Body
  }
  deriving (Show)

responseFromResult :: AppResult -> ToResponse
responseFromResult = \case
  ResText t -> r200 textHeaders $ BL.fromStrict $ T.encodeUtf8 t
  ResJSON bl -> r200 jsonHeaders bl
  ResPicture (Picture contentType pic) -> r200 (pictureHeaders contentType) pic
  where
    r200 = ToResponse HTTP.status200
    toHeaders x = [("ContentType", x)]
    textHeaders = toHeaders "text/plain; charset=utf-8"
    jsonHeaders = toHeaders "application/json"
    pictureHeaders contentType =
      toHeaders $
        "image/"
          <> fromString (map toLower $ show contentType)

responseFromError :: AppError -> ToResponse
responseFromError = \case
  AccessViolation t -> r403 t
  AdminAccessViolation -> r404 pageNotFoundMSG
  AlreadyExists t -> r409 t
  ConstraintViolation t -> r400 t
  EntityNotFound t -> r404 t
  IsNull t -> r400 t
  ParsingError t -> r400 t
  PageNotFoundError -> r404 pageNotFoundMSG
  RequestHeadersError t -> r400 t
  QParamsError -> r400 "Query parameters error."
  Unathorized t -> r401 t
  UnknwonHTTPMethod method -> r405 $ "Method " <> T.show method <> " is not allowed."
  WrongPassword -> r403 "Wrong password."
  err -> r500 $ "Internal error:" <> T.show err
  where
    pageNotFoundMSG = "Page not found."
    r400 = ToResponse HTTP.status400 [] . encodeBL
    r401 = ToResponse HTTP.status401 [] . encodeBL
    r403 = ToResponse HTTP.status403 [] . toBL
    r404 = ToResponse HTTP.status404 [] . toBL
    r405 = ToResponse HTTP.status405 [] . toBL
    r409 = ToResponse HTTP.status409 [] . toBL
    r500 = ToResponse HTTP.internalServerError500 [] . encodeBL
    toBL = fromString . T.unpack
    encodeBL = BL.fromStrict . T.encodeUtf8

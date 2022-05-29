{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Api.Picture where

import Api.Get (Gettable)
import Api.Post (Postable)
import App.AppT (Application, Env (envBody, envContentType))
import App.Error (idArityMissmatchError, requestHeadersError)
import App.Result (AppResult (ResPicture), Endpoint, toResText)
import App.Types (ID (ID))
import Control.Monad.Reader (asks)
import Data.Char (toLower)
import Data.Coerce (coerce)
import Database.Get qualified as Database
import Database.HasDatabase qualified as Database
import Database.Post qualified as Database
import Entity.Picture (Picture (Picture), PictureFormat (..))
import Extended.Text (Text)
import Extended.Text qualified as T
import HKD.HKD (Display, Front)
import Logger qualified

postPicture ::
  forall m.
  ( Application m,
    Postable m Picture
  ) =>
  Endpoint m
postPicture _ = do
  Logger.info "Attempt to post picture."
  body <- asks envBody
  conentType <- asks envContentType >>= maybe err parseFormat
  toResText =<< Database.postEntity (Picture conentType body)
  where
    err = requestHeadersError "No content-type header."

getPicture ::
  forall m.
  ( Application m,
    Gettable m Picture (Front Display),
    Database.FromRowOf m (Picture (Front Display))
  ) =>
  Endpoint m
getPicture [pictureID] = do
  Logger.info "Attempt to get picture."
  ResPicture <$> Database.getEntity @Picture [coerce pictureID]
getPicture _ = idArityMissmatchError "getPicture"

parseFormat :: Application m => Text -> m PictureFormat
parseFormat t = case T.stripPrefix "image/" $ T.map toLower t of
  Nothing -> requestHeadersError $ "Wrong content-type(" <> t <> ")"
  Just "jpeg" -> pure JPEG
  Just "png" -> pure PNG
  Just "gif" -> pure GIF
  _ -> requestHeadersError "Unknown picture format."

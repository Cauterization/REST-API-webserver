module Api.Picture where

import Api.Get (Gettable)
import Api.Post (Postable)
import App.Internal
  ( AppError (RequestHeadersErr),
    Application,
    Env (envBody, envContentType),
    entityIDArityMissmatch,
  )
import App.Result (AppResult (ResPicture), Endpoint, text)
import App.Types (ID (ID))
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.Reader (asks)
import Data.Char (toLower)
import Data.Coerce (coerce)
import Database.Database qualified as Database
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
  text =<< Database.postEntity (Picture conentType body)
  where
    err = throwM $ RequestHeadersErr "No content-type header."

getPicture ::
  forall m.
  ( Application m,
    Gettable m Picture (Front Display)
  ) =>
  Endpoint m
getPicture [pictureID] = do
  Logger.info "Attempt to get picture."
  ResPicture <$> Database.getEntityGeneric @Picture [coerce pictureID]
getPicture _ = entityIDArityMissmatch $ "getPicture"

parseFormat :: Application m => Text -> m PictureFormat
parseFormat t = case T.stripPrefix "image/" $ T.map toLower t of
  Nothing -> throwM $ RequestHeadersErr $ "Wrong content-type(" <> t <> ")"
  Just "jpeg" -> pure JPEG
  Just "png" -> pure PNG
  Just "gif" -> pure GIF
  _ -> throwM $ RequestHeadersErr "Unknown picture format."

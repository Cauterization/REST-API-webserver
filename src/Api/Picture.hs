module Api.Picture where

import Api.User qualified as User

import Control.Monad.Reader
import Crypto.Hash qualified as Crypto

import Control.Lens

import Data.Maybe
import Data.List

import HKD.HKD

import Extended.Text (Text)
import Extended.Text qualified as T
import Entity.Author
import Entity.User
import Entity.Internal
import Entity.Tag
import Entity.Picture 
import App.Router
import App.Internal
import App.Types
import App.Result
import App.ResultJSON
import Api.Get
import Api.Post
import Api.Put
import Api.User


import Database.Database qualified as Database
import Database.Database (Database)
import Logger qualified
import Logger ((.<))
import Data.Coerce
import Data.Aeson qualified as A
import Data.String (IsString(fromString))
import Control.Monad.Catch

postPicture :: forall m.
    ( Application m
    , Postable m Picture
    ) => Endpoint m
postPicture _ = do
    Logger.info "Attempt to post picture."
    body       <- asks envBody
    conentType <- asks envContentType >>= maybe err parseFormat
    Logger.error $ T.show conentType
    text =<< Database.postEntity (Picture conentType body)
  where
    err = throwM $ RequestHeadersErr "No content-type header."

getPicture :: forall m.
    ( Application m
    , Gettable m Picture (Front Display)
    ) => Endpoint m
getPicture [pictureID] = do
    Logger.info "Attempt to get picture."
    ResPicture <$> Database.getEntityGeneric @Picture [coerce pictureID]
getPicture _ = entityIDArityMissmatch $ "getPicture"

parseFormat :: Application m => Text -> m PictureFormat
parseFormat t = case T.stripPrefix "image/" t of
    Nothing     -> throwM $ RequestHeadersErr "Wrong content-type."
    Just "jpeg" -> pure JPEG
    Just "png"  -> pure PNG
    Just "gif"  -> pure GIF
    _           -> throwM $ RequestHeadersErr "Unknown picture format."
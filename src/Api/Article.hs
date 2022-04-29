{-# LANGUAGE ViewPatterns #-}

module Api.Article where

import Api.User qualified as User

import Control.Monad.Reader
import Crypto.Hash qualified as Crypto

import Control.Lens

import Data.Maybe
import Data.List
import Data.Char

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
import Entity.Article
import Entity.Draft

import Database.Database qualified as Database
import Database.Database (Database)
import Logger qualified
import Logger ((.<))
import Data.Coerce
import Data.Aeson qualified as A
import Data.String (IsString(fromString))
import Control.Monad.Catch

getArticles :: forall m.
    ( Application m
    , Gettable m (Entity Article) (Front Display)
    ) => Endpoint m
getArticles _ = do
    Logger.info "Attempt to get articles"
    filters  <- getFilters @(Entity Article) @(Front Display)
    ordering <- getParam "sort"
    json =<< Database.getEntitiesWith @(Entity Article) @(Front Display)
        filters
        (const $ articlesGetQuery $ maybe "" applyOrder ordering)

applyOrder :: IsString s => Text -> s
applyOrder (T.break (== ',') -> (field, direction)) 
    = fromString $  "ORDER BY " <> fieldOrder <> " " <> directionOrder
  where
    fieldOrder = case T.map toLower field of
        "date"     -> "created"
        "author"   -> "login"
        "category" -> "category[1]" 
        "photos"   -> "array_length(pics,1)"
        _          -> "id"
    directionOrder = case T.map toLower direction of
        ",desc"     -> "DESC"
        _          -> ""


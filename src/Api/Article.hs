{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Api.Article where

import Api.Get (Gettable, getFilters)
import App.Internal (Application, getParam)
import App.Result (Endpoint)
import App.ResultJSON (json)
import Data.Char (toLower)
import Data.String (IsString (fromString))
import Database.Get qualified as Database
import Entity.Article (Article, articlesGetQuery)
import Entity.Internal (Entity)
import Extended.Text (Text)
import Extended.Text qualified as T
import HKD.HKD (Display, Front)
import Logger qualified

getArticles ::
  forall m.
  ( Application m,
    Gettable m (Entity Article) (Front Display)
  ) =>
  Endpoint m
getArticles _ = do
  Logger.info "Attempt to get articles"
  filters <- getFilters @(Entity Article) @(Front Display)
  ordering <- getParam "sort"
  json
    =<< Database.getEntitiesWith @(Entity Article) @(Front Display)
      filters
      (const $ articlesGetQuery $ maybe "" applyOrder ordering)

applyOrder :: IsString s => Text -> s
applyOrder (T.break (== ',') -> (field, direction)) =
  fromString $ "ORDER BY " <> fieldOrder <> " " <> directionOrder
  where
    fieldOrder = case T.map toLower field of
      "date" -> "created"
      "author" -> "login"
      "category" -> "category[1]"
      "photos" -> "array_length(pics,1)"
      _ -> "id"
    directionOrder = case T.map toLower direction of
      ",desc" -> "DESC"
      _ -> ""

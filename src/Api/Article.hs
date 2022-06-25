module Api.Article where

import Api.Get (Gettable, getFilters)
import App.AppT (Application)
import App.Getters (getParam)
import App.Result (Endpoint)
import App.ResultJSON (json)
import Database.Article qualified as Database
import Entity.Article (Article, defaultOrder, parseOrder)
import Entity.Internal (Entity)
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
  json =<< Database.getArticles filters (maybe defaultOrder parseOrder ordering)

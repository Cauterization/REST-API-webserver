{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Database.Article where

import Database.EntityFilters (EntityFilterParam)
import Database.Get (getEntitiesWith)
import Database.HasDatabase (HasDatabase (FromRowOf, ToRowOf))
import Entity.Article (Article, ArticleOrder, articlesGetQuery)
import Entity.Internal (Entity)
import HKD.HKD (Display, Front)

getArticles ::
  ( HasDatabase m,
    ToRowOf m [EntityFilterParam],
    FromRowOf m (Entity Article (Front Display))
  ) =>
  [EntityFilterParam] ->
  ArticleOrder ->
  m [Entity Article (Front Display)]
getArticles fs order =
  getEntitiesWith @(Entity Article) @(Front Display)
    fs
    (const $ articlesGetQuery order)

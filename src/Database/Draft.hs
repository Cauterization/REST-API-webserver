{-# LANGUAGE TypeApplications #-}

module Database.Draft where

import App.Types (ID, Token)
import Database.EntityFilters (EntityFilterParam)
import Database.Get (Gettable (entityFiltersQuery), getEntitiesWith)
import Database.HasDatabase (HasDatabase (FromRowOf, ToRowOf))
import Database.Internal (getSingle)
import Database.Post (postEntityWith)
import Entity.Draft (Draft)
import Entity.Internal (Entity)
import HKD.HKD (Create, Display, Front)

postDraft ::
  ( HasDatabase m,
    ToRowOf m (Draft Create),
    FromRowOf m (ID (Draft Create))
  ) =>
  Draft Create ->
  m (ID (Draft Create))
postDraft = postEntityWith id

getDraftByID ::
  ( HasDatabase m,
    ToRowOf m [ID (Entity Draft (Front Display))],
    FromRowOf m (Entity Draft (Front Display))
  ) =>
  ID (Entity Draft (Front Display)) ->
  m (Entity Draft (Front Display))
getDraftByID draftID =
  getSingle
    =<< getEntitiesWith
      [draftID]
      (<> " AND id = ?")

getDraftsByToken ::
  ( HasDatabase m,
    ToRowOf m (Token, EntityFilterParam, EntityFilterParam),
    FromRowOf m (Entity Draft (Front Display))
  ) =>
  Token ->
  EntityFilterParam ->
  EntityFilterParam ->
  m [Entity Draft (Front Display)]
getDraftsByToken token limit offset =
  getEntitiesWith
    (token, limit, offset)
    (<> "AND token = ? " <> entityFiltersQuery @(Entity Draft) @(Front Display))

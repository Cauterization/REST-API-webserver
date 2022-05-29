{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module Api.Draft where

import Api.Get (Gettable, getLimit, getOffset)
import Api.Post (Postable)
import Api.User (getCurrentUser)
import Api.User qualified as User
import App.AppT (Application, Env (envPath))
import App.Error (accessViolationError)
import App.Getters (decodedBody, getToken)
import App.Impure (Impure (getCurrentDate))
import App.Path (getURL)
import App.Result (Endpoint, toResText)
import App.ResultJSON (json)
import App.Router (Middleware)
import App.Types (ID (ID), Token)
import Control.Monad.Reader (asks, when)
import Data.Coerce (coerce)
import Database.Draft qualified as Database
import Database.EntityFilters qualified as Database
import Database.HasDatabase qualified as Database
import Entity.Article (Article (..))
import Entity.Author (Author (user))
import Entity.Draft (Draft (..))
import Entity.Internal (Entity (..))
import Entity.User (User)
import Extended.Text qualified as T
import HKD.HKD (Create, Display, Front)
import Logger qualified

postDraft ::
  forall m.
  ( Application m,
    Postable m Draft,
    Gettable m (Entity User) (Front Display),
    Database.ToRowOf m [Token]
  ) =>
  Endpoint m
postDraft _ = do
  Logger.info "Attempt to post draft"
  Draft Article {..} <- decodedBody @(Draft (Front Create))
  now <- getCurrentDate
  Entity {entityID = userID} <- getCurrentUser @(Front Display)
  let draft =
        Draft
          Article
            { category = coerce category,
              tags = coerce tags,
              created = now,
              author = coerce userID,
              pics = map coerce pics,
              ..
            }
  toResText =<< Database.postDraft draft

getDrafts ::
  forall m.
  ( Application m,
    Database.ToRowOf m (Token, Database.EntityFilterParam, Database.EntityFilterParam),
    Gettable m (Entity Draft) (Front Display)
  ) =>
  Endpoint m
getDrafts _ = do
  Logger.info "Attempt to get drafts"
  limit <- getLimit
  offset <- getOffset
  token <- getToken
  json =<< Database.getDraftsByToken token limit offset

draftAccess ::
  forall m.
  ( Application m,
    Gettable m (Entity User) (Front Display),
    Database.ToRowOf m [Token],
    Gettable m (Entity Draft) (Front Display)
  ) =>
  Middleware m
draftAccess ma =
  extractDraftID . getURL <$> asks envPath >>= \case
    Just draftID -> do
      Entity {entityID = userID} <- User.getCurrentUser @(Front Display)
      e <- Database.getDraftByID draftID
      when ((entityID . user . entity . author . unDraft . entity) e /= coerce userID) $
        accessViolationError "You have no access to this draft."
      ma
    _ -> ma
  where
    extractDraftID path = case map T.read . drop 1 $ dropWhile (/= "drafts") path of
      Right draftID : _ -> Just draftID
      _ -> Nothing

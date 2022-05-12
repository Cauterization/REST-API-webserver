module Api.Draft where

import Api.Get (Gettable, getLimit, getOffset)
import Api.Post (Postable)
import Api.User (getCurrentUser)
import Api.User qualified as User
import App.Internal
  ( Application,
    Env (envPath),
    Impure (getCurrentDate),
    decodedBody,
    getToken, accessViolationError
  )
import App.Result (Endpoint, text)
import App.ResultJSON (json)
import App.Router (Middleware)
import App.Types (ID (ID), Token, getURL)
import Control.Monad.Reader (asks, when)
import Data.Coerce (coerce)
import Database.Database (Database)
import Database.Database qualified as Database
import Entity.Article
  ( Article
      ( Article,
        author,
        category,
        content,
        created,
        pics,
        tags,
        title
      ),
  )
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
    Database.ToRowOf (Database m) [Token]
  ) =>
  Endpoint m
postDraft _ = do
  Logger.info "Attempt to post draft"
  Draft Article {..} <- decodedBody @(Draft (Front Create))
  now <- getCurrentDate
  Entity {entityID = userID} <- getCurrentUser @(Front Display)
  text
    =<< Database.postEntityWith @Draft @m
      id
      ( Draft
          Article
            { category = coerce category,
              tags = coerce tags,
              created = now,
              author = coerce userID,
              pics = map coerce pics,
              ..
            }
      )

getDrafts ::
  forall m.
  ( Application m,
    Database.ToRowOf
      (Database m)
      (Token, Database.EntityFilterParam, Database.EntityFilterParam),
    Gettable m (Entity Draft) (Front Display)
  ) =>
  Endpoint m
getDrafts _ = do
  Logger.info "Attempt to get drafts"
  limit <- getLimit
  offset <- getOffset
  token <- getToken
  json
    =<< Database.getEntitiesWith @(Entity Draft) @(Front Display)
      (token, limit, offset)
      (<> "AND token = ? " <> Database.entityFiltersQuery @(Entity Draft) @(Front Display))

draftAccess ::
  forall m.
  ( Application m,
    Gettable m (Entity User) (Front Display),
    Database.ToRowOf (Database m) [Token],
    Gettable m (Entity Draft) (Front Display)
  ) =>
  Middleware m
draftAccess ma =
  extractDraftID . getURL <$> asks envPath >>= \case
    Just draftID -> do
      Entity {entityID = userID} <- User.getCurrentUser @(Front Display)
      e <-
        Database.getSingle
          =<< Database.getEntitiesWith
            @(Entity Draft)
            @(Front Display)
            @[ID (Entity Draft (Front Display))]
            [draftID]
            (<> " AND id = ?")
      when ((entityID . user . entity . author . unDraft . entity) e /= coerce userID) $
        accessViolationError "You have no access to this draft."
      ma
    _ -> ma
  where
    extractDraftID path = case map T.read . drop 1 $ dropWhile (/= "drafts") path of
      Right draftID : _ -> Just draftID
      _ -> Nothing

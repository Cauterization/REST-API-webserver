module Api.Draft where

import Api.User qualified as User

import Control.Monad.Reader
import Crypto.Hash qualified as Crypto

import Control.Lens

import Data.Maybe

import HKD.HKD

import Extended.Text (Text)
import Extended.Text qualified as T
import Entity.Author
import Entity.User
import Entity.Internal
import Entity.Tag
import App.Router
import App.Internal
import App.Types
import App.Result
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

-- postEntity :: forall (e :: Type -> Type) m. 
--     ( Application m
--     , Postable m e
--     , FromJSON (e (Front Create))
--     , CreateFromFront e
--     ) => Endpoint m
-- postEntity _ = do
--     Logger.info $ "Attempt to post " <> nameOf @e
--     e <- fromFront =<< decodedBody @(e (Front Create))
--     Database.postEntity @e @m e >>= text

postDraft :: forall m.
    ( Application m
    , Postable m Draft
    , Gettable m (Entity User) (Front Display)
    , Database.ToRowOf (Database m) [Token]
    ) => Endpoint m
postDraft _ = do
    Logger.info "Attempt to post draft"
    Draft Article{..}         <- decodedBody @(Draft (Front Create))
    now                       <- getCurrentDate
    Entity{entityID = userID} <- getCurrentUser @(Front Display)
    text =<< Database.postEntityWith @Draft @m 
        id
        (Draft Article
            { category = coerce category
            , tags = coerce tags
            , created = now
            , author = coerce userID
            , ..
            })

getDrafts :: forall m.
    ( Application m
    , Database.ToRowOf (Database m) 
        (Token, Database.EntityFilterParam, Database.EntityFilterParam)
    , Gettable m (Entity Draft) (Front Display)
    ) => Endpoint m 
getDrafts _ = do
    Logger.info "Attempt to get drafts"
    limit  <- getLimit
    offset <- getOffset
    token  <- getToken
    json =<< Database.getEntitiesWith @(Entity Draft) @(Front Display) 
        (token, limit, offset) 
        (<> "AND token = ? " <> Database.entityFiltersQuery @(Entity Draft) @(Front Display))

getDraft :: forall m.
    ( Application m
    , Gettable m (Entity User) (Front Display)
    , Database.ToRowOf (Database m) [Token]
    , Gettable m (Entity Draft) (Front Display)
    ) => Endpoint m 
getDraft [draftID] = do
    Logger.info "Attempt to get drafts"
    checkDraftAccess draftID >>= json
getDraft _ = entityIDArityMissmatch "get draft"

putDraft :: forall m.
    ( Application m
    , Puttable m (Entity Draft) (Front Update)
    , Gettable m (Entity Draft) (Front Display)
    , Gettable m (Entity User) (Front Display)
    , Database.ToRowOf (Database m) [Token]
    ) => Endpoint m
putDraft [draftID] = do
    Logger.info "Attempt to put draft"
    checkDraftAccess draftID
    body <- asks envBody
    case A.eitherDecode @(Draft (Front Update)) body of
        Right d@(Draft Article{..}) -> do
            Database.putEntity (Entity (coerce draftID) d)
            text @_ @Text "Successfuly updated."
        Left err -> text err
  where
    -- cast = coerce @_ @(ID (Draft (Front Update)))
putDraft _ = entityIDArityMissmatch "put draft"

checkDraftAccess :: forall m a.
    ( Application m
    , Gettable m (Entity User) (Front Display)
    , Database.ToRowOf (Database m) [Token]
    , Gettable m (Entity Draft) (Front Display)
    ) => ID a -> m (Entity Draft (Front Display))
checkDraftAccess draftID = do
    Entity{entityID = userID} <- User.getCurrentUser @(Front Display)
    e <- Database.getSingle =<< Database.getEntitiesWith 
        @(Entity Draft) 
        @(Front Display) 
        @m 
        @[ID (Entity Draft (Front Display))]
        [coerce draftID]
        (<> " AND id = ?")
    when ((entityID . user . entity . author . unDraft . entity) e /= coerce userID) 
        $ throwM $ AccessViolation "You have no access to this draft."
    pure e

-- putEntity :: forall (e :: Type -> Type) m. 
--     ( Application m
--     , Puttable m (Entity e) (Front Update)
--     , FromJSON (e (Front Update))
--     , Typeable e
--     ) => Endpoint m
-- putEntity [eID] = do
--     Logger.info $ "Attempt to update " <> nameOf @(Entity e)
--     e <- decodedBody @(e (Front Update))
--     Database.putEntity @e @m @(Front Update) (Entity (coerce eID) e)
--     Logger.info $ nameOf @(Entity e) <> " was found."
--     text @_ @String "Successfuly updated."
-- putEntity _ = entityIDArityMissmatch $ "putEntity " .< nameOf @(Entity e)
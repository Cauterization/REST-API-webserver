module Api.Draft where

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
            , pics = map coerce pics
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
  
draftAccess :: forall m.
    ( Application m
    , Gettable m (Entity User) (Front Display)
    , Database.ToRowOf (Database m) [Token]
    , Gettable m (Entity Draft) (Front Display)
    ) => Middleware m
draftAccess ma = extractDraftID . getURL <$> asks envPath >>= \case
    Just draftID -> do
        Entity{entityID = userID} <- User.getCurrentUser @(Front Display)
        e <- Database.getSingle =<< Database.getEntitiesWith 
            @(Entity Draft) 
            @(Front Display) 
            @[ID (Entity Draft (Front Display))]
            [draftID]
            (<> " AND id = ?")
        when ((entityID . user . entity . author . unDraft . entity) e /= coerce userID) 
            $ throwM $ AccessViolation "You have no access to this draft."
        ma
    _      -> ma
  where
    extractDraftID path = case map T.read . drop 1 $ dropWhile  (/= "drafts") path of
        Right draftID : _ -> Just draftID
        _                 -> Nothing

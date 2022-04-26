module Api.Draft where

import Control.Monad.Catch
import Control.Monad.Writer
import Crypto.Hash qualified as Crypto


import HKD.HKD

import Extended.Text (Text)
import Extended.Text qualified as T
import Entity.Author
import Entity.Internal
import App.Router
import App.Internal
import App.Types
import App.Result
import Api.Get
import Entity.Article

import Database.Database qualified as Database
import Database.Database (Database)
import Logger qualified
import Logger ((.<))
import Data.Coerce
import Data.String (IsString(fromString))

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
        (<> " " <> Database.entityFiltersQuery @(Entity Draft) @(Front Display))

getDraft :: forall m.
    ( Application m
    , Database.ToRowOf (Database m) (Token, ID (Draft (Front Display)))
    , Gettable m (Entity Draft) (Front Display)
    ) => Endpoint m 
getDraft [dID] = do
    Logger.info "Attempt to get drafts"
    token <- getToken
    json =<< Database.getSingle =<< 
        Database.getEntitiesWith @(Entity Draft) @(Front Display) 
            (token, coerce @_ @(ID (Draft (Front Display))) dID)
            (<> " AND id = ?")
getDraft _ = entityIDArityMissmatch "get draft"
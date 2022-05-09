module Helpers.DraftDB where

import App.Types
import App.Internal

import Control.Monad.Catch
import Control.Monad.State
import Control.Lens

import Data.Coerce
import Data.Data
import Database.Database (EntityFilterParam)
import Database.Database qualified as Database
import Data.IntMap qualified as IM

import Entity.Article
import Entity.Author
import Entity.Category
import Entity.Draft
import Entity.User
import Entity.Internal

import Extended.Text (Text)

import Helpers.Draft
import Helpers.AuthorDB
import Helpers.CategoryDB
import Helpers.TagDB
import Helpers.Database
import Helpers.Monad
import Helpers.UserDB
import Helpers.Internal

import HKD.HKD
import Data.Maybe

import Unsafe.Coerce

instance TestEntity (Draft Display) where

    toFrontCreate (Draft Article{..}) = Draft Article
        { created = Nothing
        , author  = Nothing
        , category = coerce $ entityID category
        , tags = map (coerce . entityID) tags
        , pics = map coerce pics
        , ..
        }

    withTestDatabase db = tsDraftDB .~ db

    extractTestDatabaseFromTestState = _tsDraftDB

    putDatabase db = modify (tsDraftDB .~ db) 

instance TestEntity (Draft Create) where

    fromDisplay (Draft Article{..}) = Draft Article
        { author = coerce $ entityID author 
        , category = coerce $ entityID category
        , tags = map (coerce . entityID) tags
        , pics = map coerce pics
        , ..
        }

    toDisplay (Draft Article{..}) = Draft Article
        { author = undefined
        , category = undefined
        , tags = undefined
        , pics = map coerce pics
        , ..
        }

    getTestDatabase = gets _tsDraftDB

    toFrontCreate (Draft Article{..}) = Draft Article
        { created = Nothing
        , author  = Nothing
        , category = coerce category
        , tags = map coerce tags
        , pics = map coerce pics
        , ..
        }

    alreadyExists _ _ = False

    withTestDatabase db = tsDraftDB .~ db

    extractTestDatabaseFromTestState = _tsDraftDB

instance TestEntity (Entity Draft (Front Display)) where

    getFromTestDatabase = do
        res <- getManyOrSingle
        t <- gets _tsToken
        dbU <- gets _tsUserDB
        case IM.toList $ IM.filter ((== t) . Just . token) dbU of
            [] -> pure []
            (uID, _) :_ -> pure $ filter ((== ID uID) . entityID . user . entity . author . unDraft . entity) res


instance TestEntity (Draft (Front Display)) where

    fromDisplay (Draft Article{..}) =  Draft Article
        { author = Entity (coerce $ entityID author) $ fromDisplay $ entity author
        , category = let Category{..} = entity category
            in Entity (coerce $ entityID category) Category{name = coerce name, parent = []}
        , tags = map (\Entity{..} -> Entity{entityID = coerce entityID, entity = unsafeCoerce entity}) tags 
        , pics = map coerce pics
        , ..
        }

-- | Get drafts

instance TestEntity (Token, EntityFilterParam, EntityFilterParam) where

    putToTestState (t, fl, fo) = do
        modify $ tsToken ?~ t
        modify $ tsGetFilters .~ [fl, fo]

-- | put Draft

instance TestEntity (Entity Draft (Front Update)) where

    putIntoTestDatabase = putEntityIntoTestDatabase

instance TestUpdate (Draft (Front Update)) where
    testUpdate (Draft du) (Draft dd) = Draft $ Article
        { title    = fromMaybe (title dd)   (title du)
        , content  = fromMaybe (content dd) (content du)
        , pics     = pics dd
        , created  = created dd
        , author   = author dd
        , category = category dd
        , tags     = tags dd
        }

-- | Publish draft

instance TestEntity (Entity Draft Publish) where

    putIntoTestDatabase (Entity (ID draftID) _) = do
        dbD <- gets _tsDraftDB
        case IM.lookup draftID dbD of
            Nothing -> return 0 -- throwM Database.EntityNotFound ""
            Just (Draft Article{..}) -> do
                modify $ tsDraftDB %~ IM.delete draftID
                len <- gets (IM.size . _tsArticleDB)
                modify $ tsArticleDB %~ IM.insert len Article{..}
                return 1



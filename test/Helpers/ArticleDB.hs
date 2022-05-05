{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
module Helpers.ArticleDB where

import App.Types
import App.Internal

import Control.Monad.Catch
import Control.Monad.State
import Control.Lens

import Data.Coerce
import Data.Data
import Database.Database
import Data.IntMap qualified as IM

import Entity.Article
import Entity.User
import Entity.Internal

import Extended.Text (Text)

import Helpers.Article
import Helpers.AuthorDB
import Helpers.CategoryDB
import Helpers.TagDB
import Helpers.Database
import Helpers.Monad
import Helpers.UserDB

import HKD.HKD
import Data.Maybe

instance TestEntity (Article Display) where

    withTestDatabase db = tsArticleDB .~ db

    extractTestDatabaseFromTestState = _tsArticleDB

instance TestEntity (Article (Front Display)) where

    -- fromDisplay Article{..} = Article
    --     { author = fromDisplay $ entity author
    --     , category = fromDisplay $ entity category
    --     , tags = map (fromDisplay . entity) tags
    --     , ..
    --     }

instance TestEntity (Entity Article (Front Display)) where

    getFromTestDatabase = getManyOrSingle


-- instance (TestEntity (Entity Author (Front Display))) where

--     getFromTestDatabase = getManyOrSingle 

-- instance TestEntity (Author (Front Display)) where

--     fromDisplay Author{user = Entity{..}, ..} 
--         = Author{user = Entity{ entityID = coerce entityID
--                               , entity = fromDisplay entity}
--                               , ..}

-- instance TestEntity (Author Display) where

--     extractTestDatabaseFromTestState = _tsAuthorDB

--     alreadyExists a db = let f = entityID . user 
--         in not $ IM.null $ IM.filter ((== f a) . f) db

--     getTestDatabase = gets _tsAuthorDB

--     putDatabase db = modify (tsAuthorDB .~ db)

--     withTestDatabase db = tsAuthorDB .~ db

--     toFrontCreate Author{..} = Author{user = coerce $ entityID user, ..}
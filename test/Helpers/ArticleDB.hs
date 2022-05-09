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
import Entity.Category
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

import Unsafe.Coerce

instance TestEntity (Article Display) where

    withTestDatabase db = tsArticleDB .~ db

    extractTestDatabaseFromTestState = _tsArticleDB

instance TestEntity (Article (Front Display)) where

    fromDisplay Article{..} = Article
        { author = Entity (coerce $ entityID author) $ fromDisplay $ entity author
        , category = Entity (coerce $ entityID category) $ fromDisplay $ entity category
        , tags = map (\(Entity i e) -> Entity (coerce i) (fromDisplay e)) tags
        , pics = map coerce pics
        , .. 
        }

instance TestEntity (Entity Article (Front Display)) where

    getFromTestDatabase = getManyOrSingle

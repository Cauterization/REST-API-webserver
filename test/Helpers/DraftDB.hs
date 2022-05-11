module Helpers.DraftDB where

import App.Types ( ID(ID), Token )
import Control.Lens ( (%~), (.~), (?~) )
import Control.Monad.State ( gets, modify )
import Data.Coerce ( coerce )
import Data.IntMap qualified as IM
import Data.Maybe ( fromMaybe )
import Database.Database (EntityFilterParam)
import Database.Database qualified as Database
import Entity.Article
    ( Article(Article, created, author, category, tags, pics, content,
              title) )
import Entity.Author ( Author(user) )
import Entity.Category
    ( Category(Category, name, parent), CategoryName(CategoryName) )
import Entity.Draft ( Draft(..) )
import Entity.Internal ( Entity(..) )
import Entity.User ( User(token) )
import Extended.Text (Text)
import HKD.HKD ( Publish, Front, Update, Create, Display )
import Helpers.AuthorDB ()
import Helpers.Database
    ( TestEntity(putIntoTestDatabase, putToTestState,
                 getFromTestDatabase, alreadyExists, getTestDatabase, toDisplay,
                 fromDisplay, toFrontCreate, putDatabase,
                 extractTestDatabaseFromTestState, withTestDatabase),
      TestUpdate(..),
      getManyOrSingle,
      putEntityIntoTestDatabase )
import Helpers.Monad
    ( TestState(_tsArticleDB, _tsToken, _tsUserDB, _tsDraftDB),
      tsArticleDB,
      tsDraftDB,
      tsGetFilters,
      tsToken )
import Unsafe.Coerce ( unsafeCoerce )

instance TestEntity (Draft Display) where
  toFrontCreate (Draft Article {..}) =
    Draft
      Article
        { created = Nothing,
          author = Nothing,
          category = coerce $ entityID category,
          tags = map (coerce . entityID) tags,
          pics = map coerce pics,
          ..
        }

  withTestDatabase db = tsDraftDB .~ db

  extractTestDatabaseFromTestState = _tsDraftDB

  putDatabase db = modify (tsDraftDB .~ db)

instance TestEntity (Draft Create) where
  fromDisplay (Draft Article {..}) =
    Draft
      Article
        { author = coerce $ entityID author,
          category = coerce $ entityID category,
          tags = map (coerce . entityID) tags,
          pics = map coerce pics,
          ..
        }

  toDisplay (Draft Article {..}) =
    Draft
      Article
        { author = undefined,
          category = undefined,
          tags = undefined,
          pics = map coerce pics,
          ..
        }

  getTestDatabase = gets _tsDraftDB

  toFrontCreate (Draft Article {..}) =
    Draft
      Article
        { created = Nothing,
          author = Nothing,
          category = coerce category,
          tags = map coerce tags,
          pics = map coerce pics,
          ..
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
      (uID, _) : _ -> pure $ filter ((== ID uID) . entityID . user . entity . author . unDraft . entity) res

instance TestEntity (Draft (Front Display)) where
  fromDisplay (Draft Article {..}) =
    Draft
      Article
        { author = Entity (coerce $ entityID author) $ fromDisplay $ entity author,
          category =
            let Category {..} = entity category
             in Entity (coerce $ entityID category) Category {name = coerce name, parent = []},
          tags = map (\Entity {..} -> Entity {entityID = coerce entityID, entity = unsafeCoerce entity}) tags,
          pics = map coerce pics,
          ..
        }

-- | Get draft
instance TestEntity (Token, EntityFilterParam, EntityFilterParam) where
  putToTestState (t, fl, fo) = do
    modify $ tsToken ?~ t
    modify $ tsGetFilters .~ [fl, fo]

-- | Put Draft
instance TestEntity (Entity Draft (Front Update)) where
  putIntoTestDatabase = putEntityIntoTestDatabase

instance TestUpdate (Draft (Front Update)) where
  testUpdate (Draft du) (Draft dd) =
    Draft $
      Article
        { title = fromMaybe (title dd) (title du),
          content = fromMaybe (content dd) (content du),
          pics = pics dd,
          created = created dd,
          author = author dd,
          category = category dd,
          tags = tags dd
        }

-- | Publish draft
instance TestEntity (Entity Draft Publish) where
  putIntoTestDatabase (Entity (ID draftID) _) = do
    dbD <- gets _tsDraftDB
    case IM.lookup draftID dbD of
      Nothing -> return 0 
      Just (Draft Article {..}) -> do
        modify $ tsDraftDB %~ IM.delete draftID
        len <- gets (IM.size . _tsArticleDB)
        modify $ tsArticleDB %~ IM.insert len Article {..}
        return 1

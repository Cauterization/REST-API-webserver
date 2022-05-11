module Helpers.TagDB where

import Control.Lens ( (.~) )
import Control.Monad.State ( gets, modify )
import Data.Coerce ( coerce )
import Data.IntMap qualified as IM
import Data.Maybe ( fromMaybe )
import Entity.Internal ( Entity )
import Entity.Tag ( Tag(..) )
import Extended.Text (Text)
import HKD.HKD ( Front, Update, Create, Display )
import Helpers.Database
import Helpers.Monad ( TestState(_tsTagDB), tsTagDB )

-- | Post
instance TestEntity (Tag Create) where
  extractTestDatabaseFromTestState = _tsTagDB

  alreadyExists t db = not $ IM.null $ IM.filter ((== tag t) . tag) db

  getTestDatabase = gets _tsTagDB

  toFrontCreate = coerce

  fromDisplay = coerce

  toDisplay = coerce

-- | Get
instance TestEntity (Tag Display) where
  extractTestDatabaseFromTestState = _tsTagDB

  alreadyExists t db = not $ IM.null $ IM.filter ((== tag t) . tag) db

  getTestDatabase = gets _tsTagDB

  toFrontCreate = coerce

  toDisplay = coerce

  withTestDatabase db = tsTagDB .~ db

  putDatabase db = modify (tsTagDB .~ db)

instance TestEntity (Tag (Front Display)) where
  fromDisplay = coerce

instance TestEntity (Entity Tag (Front Display)) where
  getFromTestDatabase = getManyOrSingle

-- | Put
instance TestEntity (Tag (Front Update)) where
  extractTestDatabaseFromTestState = _tsTagDB

instance TestUpdate (Tag (Front Update)) where
  testUpdate tu td = Tag {tag = fromMaybe (tag td) (tag tu)}

instance TestEntity (Entity Tag (Front Update)) where
  putIntoTestDatabase = putEntityIntoTestDatabase

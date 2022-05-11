module Helpers.CategoryDB where

import Control.Lens ( (.~) )
import Control.Monad.State ( gets, modify )
import Data.Coerce ( coerce )
import Data.IntMap qualified as IM
import Data.Maybe (isNothing)
import Entity.Category ( Category(..), CategoryName(CategoryName) )
import Extended.Text qualified as T
import HKD.HKD ( Front, Create, Display )
import Helpers.Database
    ( TestEntity(putDatabase, extractTestDatabaseFromTestState,
                 toDisplay, fromDisplay, toFrontCreate, alreadyExists,
                 withTestDatabase, getTestDatabase) )
import Helpers.Monad ( TestState(_tsCatDB), tsCatDB )
import Unsafe.Coerce ( unsafeCoerce )

-- | Post
instance TestEntity (Category Create) where
  extractTestDatabaseFromTestState = _tsCatDB

  getTestDatabase = gets _tsCatDB

  withTestDatabase db = tsCatDB .~ db

  alreadyExists c db = not $ IM.null $ IM.filter ((== coerce (name c)) . name) db

  toFrontCreate = unsafeCoerce

  fromDisplay = unsafeCoerce

  toDisplay = unsafeCoerce

instance TestEntity (Category Display) where
  extractTestDatabaseFromTestState = _tsCatDB

  toFrontCreate = unsafeCoerce

  withTestDatabase db = tsCatDB .~ db

  putDatabase db = modify (tsCatDB .~ db)

instance TestEntity (Category (Front Display)) where
  fromDisplay Category {..} =
    Category
      { name = coerce name,
        parent = pure $ CategoryName $ T.show parent
      }

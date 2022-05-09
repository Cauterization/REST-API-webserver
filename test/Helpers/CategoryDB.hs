module Helpers.CategoryDB where

import App.Types
import App.Internal

import Control.Monad.Catch
import Control.Monad.State
import Control.Lens

import Data.Coerce
import Data.Functor
import Data.IntMap qualified as IM

import Database.Database

import Entity.Category
import Extended.Text qualified as T

import Helpers.Monad
import Helpers.Database

import HKD.HKD

import Unsafe.Coerce
import Data.Maybe (isNothing)

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

    fromDisplay Category{..} = Category
        { name = coerce name
        , parent = pure $ CategoryName $ T.show parent
        }

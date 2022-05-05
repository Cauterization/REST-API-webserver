module Helpers.PictureDB where

import App.Types
import App.Internal

import Control.Monad.Catch
import Control.Monad.State
import Control.Lens

import Data.Coerce
import Data.Maybe
import Data.IntMap qualified as IM
import Database.Database

import Entity.Picture
import Entity.Internal

import Extended.Text (Text)

import Helpers.Database
import Helpers.Monad

import HKD.HKD

import Unsafe.Coerce

instance TestEntity (Picture (Front Display)) where

    getFromTestDatabase = do
        db <- getDatabase @Picture
        gets _tsIDs >>= \case
            [eID] -> pure . entity <$> lookupT eID db
            x     -> error $ "getManyOrSingle" <> show x 

    fromDisplay = unsafeCoerce
    

instance TestEntity (Picture Display) where

    extractTestDatabaseFromTestState = _tsPictureDB

    putDatabase db = modify (tsPictureDB .~ db)

    withTestDatabase db = tsPictureDB .~ db

instance TestEntity (Picture Create) where

    alreadyExists _ _ = False

    getTestDatabase = gets _tsPictureDB

    toDisplay = unsafeCoerce
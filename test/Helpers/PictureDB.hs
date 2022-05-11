module Helpers.PictureDB where

import Control.Lens ( (.~) )
import Control.Monad.State ( gets, modify )
import Data.IntMap qualified as IM
import Entity.Internal ( Entity(entity) )
import Entity.Picture ( Picture )
import Extended.Text (Text)
import HKD.HKD ( Front, Create, Display )
import Helpers.Database
    ( TestEntity(toDisplay, getTestDatabase, alreadyExists,
                 withTestDatabase, putDatabase, extractTestDatabaseFromTestState,
                 getFromTestDatabase, fromDisplay),
      lookupT,
      getDatabase )
import Helpers.Monad
    ( TestState(_tsPictureDB, _tsIDs), tsPictureDB )
import Unsafe.Coerce ( unsafeCoerce )

instance TestEntity (Picture (Front Display)) where
  getFromTestDatabase = do
    db <- getDatabase @Picture
    gets _tsIDs >>= \case
      [eID] -> pure . entity <$> lookupT eID db
      x -> error $ "getManyOrSingle" <> show x

  fromDisplay = unsafeCoerce

instance TestEntity (Picture Display) where
  extractTestDatabaseFromTestState = _tsPictureDB

  putDatabase db = modify (tsPictureDB .~ db)

  withTestDatabase db = tsPictureDB .~ db

instance TestEntity (Picture Create) where
  alreadyExists _ _ = False

  getTestDatabase = gets _tsPictureDB

  toDisplay = unsafeCoerce

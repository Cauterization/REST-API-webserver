module Helpers.UserDB where

import App.Types ( ID(ID) )
import Control.Lens ( (%~), (.~) )
import Control.Monad.Catch ( MonadThrow(throwM) )
import Control.Monad.State ( gets, modify )
import Data.IntMap qualified as IM
import Data.Maybe ( fromMaybe )
import Database.Database
    ( DBError(TooManyEntities, EntityNotFound) )
import Entity.Internal ( Entity(Entity) )
import Entity.User ( User(..) )
import Extended.Text (Text)
import HKD.HKD ( Front, Update, Create, Display )
import Helpers.Database
    ( TestEntity(putIntoTestDatabase, putDatabase, withTestDatabase,
                 extractTestDatabaseFromTestState, getFromTestDatabase, fromDisplay,
                 alreadyExists, toDisplay, getTestDatabase),
      TestUpdate(..),
      putEntityIntoTestDatabase,
      getDatabase )
import Helpers.Monad
    ( TestState(_tsUserLogin, _tsToken, _tsUserDB), tsUserDB )
import Unsafe.Coerce ( unsafeCoerce )

-- | Post
instance TestEntity (User Create) where
  alreadyExists u db = not $ IM.null $ IM.filter ((== login u) . login) db

  getTestDatabase = gets _tsUserDB

  toDisplay = unsafeCoerce

instance TestEntity (User (Front Create)) where
  alreadyExists u db = not $ IM.null $ IM.filter ((== login u) . login) db

-- | Get
instance TestEntity (Entity User (Front Display)) where
  getTestDatabase = gets _tsUserDB

  getFromTestDatabase = do
    Just t <- gets _tsToken
    db <- getDatabase @User
    case IM.toList $ IM.filter ((== t) . token) db of
      [] -> throwM $ EntityNotFound ""
      [(uID, u)] -> pure [Entity (ID uID) $ fromDisplay u]
      _ -> throwM $ TooManyEntities ""

instance TestEntity (User (Front Display)) where
  fromDisplay User {..} = User {password = Nothing, token = Nothing, ..}

instance TestEntity (User Display) where
  extractTestDatabaseFromTestState = _tsUserDB

  getTestDatabase = gets _tsUserDB

  withTestDatabase db = tsUserDB .~ db

  toDisplay = id

  fromDisplay = id

  putDatabase db = modify $ tsUserDB .~ db

-- | Auth
instance TestEntity (Entity User Display) where
  getTestDatabase = gets _tsUserDB

  getFromTestDatabase = do
    l <- gets _tsUserLogin
    map (\(eID, e) -> Entity (ID eID) e) . IM.toList . IM.filter ((== l) . login)
      <$> getTestDatabase @(Entity User Display)

instance TestUpdate (User Update) where
  testUpdate uu ud = #token %~ flip fromMaybe (token uu) $ ud

instance TestEntity (Entity User Update) where
  getTestDatabase = gets _tsUserDB

  putIntoTestDatabase = putEntityIntoTestDatabase

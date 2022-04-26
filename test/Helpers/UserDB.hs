module Helpers.UserDB where

import App.Types

import Control.Monad.State
import Control.Monad.Catch
import Control.Lens

import Data.Coerce
import Data.Maybe
import Data.IntMap qualified as IM

import Database.Database

import Entity.User
import Entity.Internal
import Extended.Text (Text)

import Helpers.Database
import Helpers.Monad
import Helpers.User

import HKD.HKD

import Unsafe.Coerce

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
        db     <- getDatabase @User
        case IM.toList $ IM.filter ((== t) . token) db of
            []        -> throwM $ EntityNotFound ""
            [(uID,u)] -> pure [Entity (ID uID) $ fromDisplay u]
            _         -> throwM $ TooManyEntities ""
    
instance TestEntity (User (Front Display)) where
    fromDisplay User{..} = User{password = Nothing, token = Nothing, ..}

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
        map (\(eID,e) -> Entity (ID eID) e) . IM.toList . IM.filter ((== l) . login)
            <$> getTestDatabase @(Entity User Display)

instance TestUpdate (User Update) where
    testUpdate uu ud = #token %~ flip fromMaybe (token uu) $ ud

instance TestEntity (Entity User Update) where

    getTestDatabase = gets _tsUserDB

    putIntoTestDatabase = putEntityIntoTestDatabase

-- instance TestEntity (ID (Entity User Update)) where
--     putToTestState (ID uID) = modify $ tsIDs %~ (uID:)



-- instance ToRowOfT (User Create) where

--     getsDatabase = gets userDB

--     alreadyExists u uMap = not . null $ M.filter ((== login u) . login) uMap

--     toDisplay = pure . unsafeCoerce 

--     insertIntoTestDB uID u TestState{..} 
--         = TestState{userDB = M.insert uID u userDB, ..}   

-- instance FromRowOfT (User Display) where

-- instance FromRowOfT (ID (User Create)) where

-- instance FromRowOfT (Entity User Display) where 

--     getEntityFromTestDatabase q = do
--         Just l <- gets tsUserLogin
--         db <- gets userDB
--         pure $ map (\(uID, u) -> Entity (coerce uID) u) $ 
--             filter ((\User{..} -> login == l) . snd) $ M.toList db

-- instance FromRowOfT (User (Front Display)) where

--     getEntityFromTestDatabase q = do
--         db <- gets userDB
--         gets tsToken >>= \case
--             Just t -> pure $ map userDisplayToUserFrontDisplay 
--                 $ filter (\User{..} -> token == t) $ M.elems db

-- instance FromRowOfT (Entity User (Front Display)) where

--     getEntityFromTestDatabase q = do
--         db <- gets userDB
--         gets tsToken >>= \case
--             Just t -> pure $ map (\(uID, u) -> Entity (coerce uID)
--                 $ userDisplayToUserFrontDisplay u)
--                     $ filter ((\User{..} -> token == t) . snd) $ M.toList db

-- type UserAuthT = [Text]

-- instance ToOneRow (User (Front Update)) IDs where 

--     type instance MkOneRow (User  (Front Update)) IDs 
--         = UserAuthT
--



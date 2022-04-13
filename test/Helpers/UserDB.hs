module Helpers.UserDB where

import App.Types

import Control.Monad.State

import Data.Coerce
import Data.Map qualified as M

import Database.Database

import Entity.User
import Entity.Internal
import Extended.Text (Text)

import Helpers.Database
import Helpers.Monad
import Helpers.User

import HKD.HKD

import Unsafe.Coerce

instance ToRowOfT (User Create) where

    getsDatabase = gets userDB

    alreadyExists u uMap = not . null $ M.filter ((== login u) . login) uMap

    toDisplay = pure . unsafeCoerce 

    insertIntoTestDB uID u TestState{..} 
        = TestState{userDB = M.insert uID u userDB, ..}   

instance FromRowOfT (User Display) where

instance FromRowOfT (ID (User Create)) where

instance FromRowOfT (Entity User Display) where 

    getEntityFromTestDatabase q = do
        Just l <- gets tsUserLogin
        db <- gets userDB
        pure $ map (\(uID, u) -> Entity (coerce uID) u) $ 
            filter ((\User{..} -> login == l) . snd) $ M.toList db

instance FromRowOfT (User (Front Display)) where

    getEntityFromTestDatabase q = do
        db <- gets userDB
        gets tsToken >>= \case
            Just t -> pure $ map userDisplayToUserFrontDisplay 
                $ filter (\User{..} -> token == t) $ M.elems db

instance FromRowOfT (Entity User (Front Display)) where

    getEntityFromTestDatabase q = do
        db <- gets userDB
        gets tsToken >>= \case
            Just t -> pure $ map (\(uID, u) -> Entity (coerce uID)
                $ userDisplayToUserFrontDisplay u)
                    $ filter ((\User{..} -> token == t) . snd) $ M.toList db

type UserAuthT = [Text]

instance ToOneRow (User (Front Update)) IDs where 

    type instance MkOneRow (User  (Front Update)) IDs 
        = UserAuthT
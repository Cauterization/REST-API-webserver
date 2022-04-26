{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
module Helpers.AuthorDB where

import App.Types
import App.Internal

import Control.Monad.Catch
import Control.Monad.State
import Control.Lens

import Data.Coerce
import Data.Data
import Database.Database
import Data.IntMap qualified as IM

import Entity.Author
import Entity.User
import Entity.Internal

import Extended.Text (Text)

import Helpers.Author
import Helpers.Database
import Helpers.Monad
import Helpers.UserDB

import HKD.HKD
import Data.Maybe


-- | Post

instance TestEntity (Author Create) where

    getTestDatabase = gets _tsAuthorDB

    alreadyExists a db = not $ IM.null $ IM.filter 
        ((== coerce (user a)) . entityID . user) db

    toDisplay Author{..} = Author{user = Entity (coerce user) User{}, ..}

    toFrontCreate Author{..} = Author{user = coerce user, ..}

    fromDisplay Author{..} = Author{user = coerce $ entityID user, ..}

    extractTestDatabaseFromTestState = _tsAuthorDB

-- | Get

instance (TestEntity (Entity Author (Front Display))) where

    getFromTestDatabase = getManyOrSingle 

instance TestEntity (Author (Front Display)) where

    fromDisplay Author{user = Entity{..}, ..} 
        = Author{user = Entity{ entityID = coerce entityID
                              , entity = fromDisplay entity}
                              , ..}

instance TestEntity (Author Display) where

    extractTestDatabaseFromTestState = _tsAuthorDB

    alreadyExists a db = let f = entityID . user 
        in not $ IM.null $ IM.filter ((== f a) . f) db

    getTestDatabase = gets _tsAuthorDB

    putDatabase db = modify (tsAuthorDB .~ db)

    withTestDatabase db = tsAuthorDB .~ db

    toFrontCreate Author{..} = Author{user = coerce $ entityID user, ..}

-- Put

instance TestEntity (Author (Front Update)) where

    extractTestDatabaseFromTestState = _tsAuthorDB 

instance TestUpdate (Author (Front Update)) where

    testUpdate au ad = Author
        { description = fromMaybe (description ad) (description au)
        , user = user ad
        }

instance TestEntity (Entity Author (Front Update)) where

    putIntoTestDatabase = putEntityIntoTestDatabase

    -- putDatabase db = pure () -- modify (tsAuthorDB .~ db)

-- instance ToRowOfT (Author Create) where

--     getsDatabase = gets authorDB

--     alreadyExists a aMap = not . null $ 
--         M.filter ((== user a) . (coerce . entityID . user)) aMap

--     toDisplay a = do
--         let d = description a
--             uID = user a
--         mbUser <- gets $ M.lookup (coerce uID) . userDB
--         case mbUser of
--             Just user -> pure Author{user = Entity (coerce uID) user, description = d}
--             _ -> throwM $ EntityNotFound ""

--     fromDisplay a = Author
--         {description = description a, user = coerce entityID $ user a}

--     insertIntoTestDB aID a TestState{..} 
--         = TestState{authorDB = M.insert aID a authorDB, ..}

    
-- instance FromRowOfT (ID (Author Create)) where

-- instance ToRowOfT (Author Display) where

-- instance FromRowOfT (Author (Front Display)) where

--     getEntityFromTestDatabase q = do
--         db <- gets authorDB
--         gets ids >>= \case
--             [aID] -> case M.lookup (ID aID) db of
--                 Just a -> pure [authorDisplayToAuthorFrontDisplay a]
--                 Nothing -> throwM $ EntityNotFound ""
--             []    -> do
--                 page <- gets tsPage
--                 pag  <- gets tsPaginationSize
--                 return $ map authorDisplayToAuthorFrontDisplay 
--                     $ take pag $ drop (pag * (page - 1)) $ M.elems db

-- type AuthorUpdateT = (Maybe NotUpdated, Maybe Text, ID (Path Current))

-- instance ToRowOfT AuthorUpdateT where

--     putEntityToTestDatabase (_, desc, coerce -> aID) = do
--         db <- gets authorDB
--         case M.lookup aID db of
--             Nothing -> throwM $ EntityNotFound ""
--             Just Author{..} -> 
--                 let a' = Author{description = desc >\ description, .. }
--                     db' = M.insert aID a' db
--                 in modify $ \TestState{..} -> TestState{authorDB = db', .. }
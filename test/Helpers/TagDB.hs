{-# LANGUAGE ViewPatterns #-}

module Helpers.TagDB where

import App.Types
import App.Internal

import Control.Monad.Catch
import Control.Monad.State

import Data.Coerce
import Data.Maybe
import Data.Map qualified as M

import Entity.Tag

import Extended.Text (Text)

import Helpers.Database
import Helpers.Monad
import Helpers.Tag
import Helpers.UserDB
import Helpers.Update

import HKD.HKD

import Unsafe.Coerce


instance ToRowOfT (Tag Create) where

    getsDatabase = gets tagDB

    alreadyExists t tMap =  not . null $ M.filter ((== tag t) . tag) tMap

    toDisplay = pure . unsafeCoerce 

    fromDisplay = unsafeCoerce 

    insertIntoTestDB tID t TestState{..} 
        = TestState{tagDB = M.insert tID t tagDB, ..}

instance FromRowOfT (ID (Tag Create))

type TagUpdateT = (Maybe Text, ID (Path Current))

instance FromRowOfT (Tag (Front Display)) where
    getEntityFromTestDatabase q = do
        db <- gets tagDB
        gets ids >>= \case
            [tID] -> case M.lookup (ID tID) db of
                Just t -> pure [tagDisplayToFrontDisplay t]
                Nothing -> throwM $ EntityNotFound ""
            []    -> do
                page <- gets tsPage
                pag  <- gets tsPaginationSize
                return $ map tagDisplayToFrontDisplay 
                    $ take pag $ drop (pag * (page - 1)) $ M.elems db

-- | because of collision of update types for token and user auth, here we need to
-- redirect this thing

instance ToRowOfT TagUpdateT where

    putEntityToTestDatabase (newName, coerce -> tID) = do
        db <- gets tagDB
        case M.lookup tID db of
            Nothing -> putEntityToTestDatabase @UserAuthT [fromJust newName]
            Just Tag{..} -> 
                let t' = Tag{tag = newName >\ tag, .. }
                    db' = M.insert tID t' db
                in modify $ \TestState{..} -> TestState{tagDB = db', .. }
{-# LANGUAGE ViewPatterns #-}

module Helpers.AuthorDB where

import App.Types
import App.Internal

import Control.Monad.Catch
import Control.Monad.State

import Data.Coerce
import Data.Map qualified as M

import Entity.Author
import Entity.Internal

import Extended.Text (Text)

import Helpers.Author
import Helpers.Database
import Helpers.Monad
import Helpers.Update

import HKD.HKD

instance ToRowOfT (Author Create) where

    getsDatabase = gets authorDB

    alreadyExists a aMap = not . null $ 
        M.filter ((== user a) . (coerce . entityID . user)) aMap

    toDisplay a = do
        let d = description a
            uID = user a
        mbUser <- gets $ M.lookup (coerce uID) . userDB
        case mbUser of
            Just user -> pure Author{user = Entity (coerce uID) user, description = d}
            _ -> throwM $ EntityNotFound ""

    fromDisplay a = Author
        {description = description a, user = coerce entityID $ user a}

    insertIntoTestDB aID a TestState{..} 
        = TestState{authorDB = M.insert aID a authorDB, ..}

    
instance FromRowOfT (ID (Author Create)) where

instance ToRowOfT (Author Display) where

instance FromRowOfT (Author (Front Display)) where

    getEntityFromTestDatabase q = do
        db <- gets authorDB
        gets ids >>= \case
            [aID] -> case M.lookup (ID aID) db of
                Just a -> pure [authorDisplayToAuthorFrontDisplay a]
                Nothing -> throwM $ EntityNotFound ""
            []    -> do
                page <- gets tsPage
                pag  <- gets tsPaginationSize
                return $ map authorDisplayToAuthorFrontDisplay 
                    $ take pag $ drop (pag * (page - 1)) $ M.elems db

type AuthorUpdateT = (Maybe NotUpdated, Maybe Text, ID (Path Current))

instance ToRowOfT AuthorUpdateT where

    putEntityToTestDatabase (_, desc, coerce -> aID) = do
        db <- gets authorDB
        case M.lookup aID db of
            Nothing -> throwM $ EntityNotFound ""
            Just Author{..} -> 
                let a' = Author{description = desc >\ description, .. }
                    db' = M.insert aID a' db
                in modify $ \TestState{..} -> TestState{authorDB = db', .. }
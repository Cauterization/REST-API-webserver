module Helpers.CategoryDB where

import App.Types
import App.Internal

import Control.Monad.Catch
import Control.Monad.State

import Data.Coerce
import Data.Functor
import Data.Map qualified as M

import Entity.Category

import Helpers.Monad
import Helpers.Database

import HKD.HKD

import Unsafe.Coerce
import Data.Maybe (isNothing)

instance ToRowOfT (Category Create) where

    alreadyExists c cMap = not . null $ 
        M.filter ((== name c) . coerce . name) cMap

    getsDatabase = gets catDB

    toDisplay c = pure $ Category (coerce $ name c) (coerce $ parent c)

    fromDisplay c = Category{name = coerce $ name c, parent = coerce $ parent c}

    insertIntoTestDB сID c TestState{..} 
        = TestState{catDB = M.insert сID c catDB, ..}



instance FromRowOfT (ID (Category Create)) where

instance FromRowOfT (Category (Front Display)) where

    getEntityFromTestDatabase q = do
        db <- gets catDB
        gets ids >>= \case
            [cID] -> case M.lookup (ID cID) db of
                Just c -> pure . pure $ collectCat db [c]
                Nothing -> throwM $ EntityNotFound ""
            []    -> do
                page <- gets tsPage
                pag  <- gets tsPaginationSize
                return $ take pag $ drop (pag * (page - 1)) $ 
                    map (collectCat db . pure) $ M.elems db 

collectCat :: TDB Category -> [Category Display] -> Category (Front Display)
collectCat db (c:cs) = case parent c of
    Nothing -> Category (coerce $ name c) (map name cs)
    Just pID  -> case M.lookup pID db of
        Just p -> collectCat (M.filter ((/= Just pID) . parent) db) (p:c:cs)
        Nothing -> Category (coerce $ name c) (map name cs)




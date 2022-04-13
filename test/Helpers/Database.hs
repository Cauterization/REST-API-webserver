{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Helpers.Database where

import App.Types

import Control.Monad.Catch
import Control.Monad.State

import Data.Coerce
import Data.Data
import Data.List.Extra
import Data.Map qualified as M
import Data.String
import Data.Time qualified as Time

import Database.Database

import Entity.Author
import Entity.Category
import Entity.Tag
import Entity.User
import Entity.Internal
import Extended.Text (Text)

import HKD.HKD

import Helpers.Author
import Helpers.Tag
import Helpers.User
import Helpers.Monad
import Helpers.Update

import Test.Hspec
import Test.QuickCheck

import Unsafe.Coerce

import Api.User (mkHash)
import qualified Logger
import qualified Extended.Text as T
import Data.Maybe (fromJust)


type StateMod = TestState -> TestState

data TestDB

instance IsDatabase TestDB  where

    type QueryOf       TestDB   = TestQuery
    type ToRowOf       TestDB q = (Show q, ToRowOfT q)
    type FromRowOf     TestDB r = (Show r, FromRowOfT r)
    type ConnectionOf  TestDB   = ()
    type DatabaseMonad TestDB   = TestMonad

    runMigrations _ _ = pure ()

    mkConnectionIO _ = pure ()

    postToDatabase :: forall e. 
        ( ToRowOf TestDB e
        , FromRowOf TestDB (ID e
        )
        ) => () -> TestQuery -> e -> DatabaseMonad TestDB (ID e)
    postToDatabase _ _ e = do
        db <- getsDatabase @e
        when (alreadyExists e db) $ throwM $ AlreadyExists ""
        let eID = ID $ length (M.toList db) + 1
        eDisplay <- toDisplay e
        modify $ insertIntoTestDB @e (coerce eID) eDisplay 
        pure eID

    putIntoDatabase :: forall q. ToRowOf TestDB q 
        => () -> TestQuery -> q -> TestMonad ()
    putIntoDatabase _ _ q = putEntityToTestDatabase @q q

    getFromDatabase ::  forall q r. (ToRowOf TestDB q, FromRowOf TestDB r)
        => () -> TestQuery -> q -> TestMonad [r]
    getFromDatabase _ _ q = do
        putToState q
        getEntityFromTestDatabase @r q

    deleteFromDatabase :: forall q. ToRowOf TestDB q 
        => () -> TestQuery -> q -> TestMonad Integer
    deleteFromDatabase _ _ q = do
        putToState q
        deleteEntityFromTestDatabase @q q

class ToRowOfT q where
    alreadyExists    :: q -> DBOf q -> Bool
    toDisplay        :: q -> TestMonad (ToDisplay q)
    fromDisplay      :: ToDisplay q -> q
    getsDatabase     :: TestMonad (DBOf q)
    insertIntoTestDB :: ID (ToDisplay q) -> ToDisplay q -> StateMod
    putToState       :: q -> TestMonad ()
    deleteEntityFromTestDatabase :: q -> TestMonad Integer
    putEntityToTestDatabase :: q -> TestMonad ()
    
class FromRowOfT r where
    getEntityFromTestDatabase :: q -> TestMonad [r] 

type family DBOf e where
    DBOf e = EMap (ToDisplay e)

type family ToDisplay q where
    ToDisplay (User     a) = User   Display
    ToDisplay (Author   a) = Author Display
    ToDisplay (Tag      a) = Tag    Display
    ToDisplay (Category a) = Category Display

instance ToRowOfT [ID (Path Current)] where
    putToState ids' = modify (\TestState{..} 
        -> TestState{ids = ids ++ map idVal ids', ..})
    deleteEntityFromTestDatabase [eID] = deleteAllEntitiesWithID eID

instance ToRowOfT [Text] where
    putToState [t] =  do
        modify (\TestState{..} -> TestState{tsToken = Just t, ..})
        modify (\TestState{..} -> TestState{tsUserLogin = Just t, ..})

    -- | User auth token update
    putEntityToTestDatabase [newToken] = do
        db <- gets userDB
        mbLogin <- gets tsUserLogin
        case mbLogin of
            Nothing -> throwM $ EntityNotFound ""
            Just userLogin -> case filter ((== userLogin) . login . snd) $ M.toList db of
                [(uID, User{..})] -> let db' = M.insert uID User{token = newToken, ..} db
                    in modify (\TestState{..} -> TestState{userDB= db', ..})
            _ -> throwM $ EntityNotFound ""


instance ToRowOfT [Page] where
    putToState [p] = modify (\TestState{..} -> TestState{tsPage = p, ..})

newtype TestQuery = TestQuery () deriving Show

instance IsString TestQuery where
    fromString _ = TestQuery ()

instance Semigroup TestQuery where
    (<>) _ _ = TestQuery ()

instance Monoid TestQuery where
    mempty = TestQuery ()


instance (Show (e Create), Data (e Create), Typeable e, ToRowOfT (e Create)) 
    => PostableTo TestDB e where
instance (Show (e a), Typeable e, Data (e a), FromRowOfT (e a)) 
    => GettableFrom TestDB e a where
instance PuttableTo TestDB e where


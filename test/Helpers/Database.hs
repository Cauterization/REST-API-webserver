{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ViewPatterns #-}
module Helpers.Database where

import App.Types

import Control.Monad.Catch
import Control.Monad.State

import Data.Coerce
import Data.List.Extra
import Data.Map qualified as M
import Data.String
import Data.Time qualified as Time

import Database.Database

import Entity.Author
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
    ToDisplay (User   a) = User   Display
    ToDisplay (Author a) = Author Display
    ToDisplay (Tag    a) = Tag    Display

instance ToRowOfT [ID (Path Current)] where
    putToState ids' = modify (\TestState{..} 
        -> TestState{ids = ids ++ map idVal ids', ..})
    deleteEntityFromTestDatabase [eID] = deleteAllEntitiesWithID eID

instance ToRowOfT [Token] where
    putToState [t] =  modify (\TestState{..} -> TestState{tsToken = Just t, ..})

instance ToRowOfT [Page] where
    putToState [p] = modify (\TestState{..} -> TestState{tsPage = p, ..})

newtype TestQuery = TestQuery () deriving Show

instance IsString TestQuery where
    fromString _ = TestQuery ()

instance Semigroup TestQuery where
    (<>) _ _ = TestQuery ()

instance Monoid TestQuery where
    mempty = TestQuery ()

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


instance ToRowOfT (User Create) where

    getsDatabase = gets userDB

    alreadyExists u uMap = not . null $ M.filter ((== login u) . login) uMap

    toDisplay = pure . unsafeCoerce 

    insertIntoTestDB uID u TestState{..} 
        = TestState{userDB = M.insert uID u userDB, ..}   

instance FromRowOfT (User Display) where

instance FromRowOfT (ID (User Create)) where

instance FromRowOfT (User (Front Display)) where

    getEntityFromTestDatabase q = do
        db <- gets userDB
        gets tsToken >>= \case
            Just t -> pure $ map userDisplayToUserFrontDisplay 
                $ filter (\User{..} -> token == t) $ M.elems db

instance ToRowOfT (Tag Create) where

    getsDatabase = gets tagDB

    alreadyExists t tMap =  not . null $ M.filter ((== name t) . name) tMap

    toDisplay = pure . unsafeCoerce -- pure $ Tag name

    fromDisplay = unsafeCoerce --Tag name

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

instance ToRowOfT TagUpdateT where
    putEntityToTestDatabase (newName, coerce -> tID) = do
        db <- gets tagDB
        case M.lookup tID db of
            Nothing -> throwM $ EntityNotFound ""
            Just Tag{..} -> 
                let t' = Tag{name = newName >\ name, .. }
                    db' = M.insert tID t' db
                in modify $ \TestState{..} -> TestState{tagDB = db', .. }
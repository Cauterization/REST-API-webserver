{-# LANGUAGE InstanceSigs #-}
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
import Entity.User
import Entity.Internal
import Extended.Text (Text)

import HKD.HKD

import Helpers.Author
import Helpers.User
import Helpers.Monad

import Test.Hspec
import Test.QuickCheck

import Unsafe.Coerce

import Api.User (mkHash)

type StateMod = TestState -> TestState

withUserDatabase :: EMap (User Display) -> StateMod
withUserDatabase emap TestState{..} = TestState{userDB = emap, ..}

withAuthorDatabase :: EMap (Author Display) -> StateMod
withAuthorDatabase emap TestState{..} = TestState{authorDB = emap, ..}

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

addIDs :: [ID a] -> TestMonad ()
addIDs ids' = modify (\TestState{..} -> TestState{ids = ids ++ map idVal ids', ..})

class ToRowOfT q where
    alreadyExists    :: q -> DBOf q -> Bool
    toDisplay        :: q -> TestMonad (ToDisplay q)
    fromDisplay      :: ToDisplay q -> q
    getsDatabase     :: TestMonad (DBOf q)
    insertIntoTestDB :: ID (ToDisplay q) -> ToDisplay q -> StateMod
    -- dbFromTestState  :: TestState -> DBOf q
    putToState       :: q -> TestMonad ()
    deleteEntityFromTestDatabase :: q -> TestMonad Integer
    
class FromRowOfT r where
    getEntityFromTestDatabase :: q -> TestMonad [r]

type family DBOf e where
    DBOf e = EMap (ToDisplay e)

type family ToDisplay q where
    ToDisplay (User   a) = User   Display
    ToDisplay (Author a) = Author Display

instance ToRowOfT [ID (Path Current)] where
    putToState ids = addIDs ids
    deleteEntityFromTestDatabase [eID] = deleteAllEntitiesWithID eID

instance ToRowOfT [Token] where
    putToState [t] =  modify (\TestState{..} -> TestState{tsToken = Just t, ..})

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
        M.filter ((== user a) . (coerce .entityID . user)) aMap

    -- withDatabase db TestState{..} = TestState{authorDB = db, ..}

    toDisplay a = do
        let d = description a
            uID = user a
        mbUser <- gets $ M.lookup (coerce uID) . userDB
        case mbUser of
            Just user -> pure Author{user = Entity (coerce uID) user, description = d}
            _ -> throwM $ EntityNotFound ""

    fromDisplay a = Author{description = description a, user = coerce entityID $ user a}

    insertIntoTestDB aID a TestState{..} 
        = TestState{authorDB = M.insert aID a authorDB, ..}

    
instance FromRowOfT (ID (Author Create)) where
    -- deleteEntityFromTestDatabase = error "og"

instance ToRowOfT (Author Display) where

    fromDisplay = unsafeCoerce

    -- dbFromTestState TestState{..} = authorDB

    -- withDatabase db TestState{..} = TestState{authorDB = db, ..}

instance FromRowOfT (Author (Front Display)) where

    getEntityFromTestDatabase q = do
        db <- gets authorDB
        gets ids >>= \case
            [aID] -> case M.lookup (ID aID) db of
                Just a -> pure $ [authorDisplayToAuthorFrontDisplay a]
                Nothing -> throwM $ EntityNotFound ""
            []    -> do
                page <- gets tsPage
                pag  <- gets tsPaginationSize
                return $ map authorDisplayToAuthorFrontDisplay 
                    $ take pag $ drop (pag * (page - 1)) $ M.elems db

instance ToRowOfT (User Create) where

    getsDatabase = gets userDB

    alreadyExists u uMap = not . null $ M.filter ((== login u) . login) uMap

    -- withDatabase db TestState{..} = TestState{userDB = db, ..}

    toDisplay = pure . unsafeCoerce 

    insertIntoTestDB uID u TestState{..} 
        = TestState{userDB = M.insert uID u userDB, ..}   

instance FromRowOfT (User Display) where

    -- getEntityFromTestDatabase = undefined

instance FromRowOfT (ID (User Create)) where

instance FromRowOfT (User (Front Display)) where

    getEntityFromTestDatabase q = do
        db <- gets userDB
        gets tsToken >>= \case
            Just t -> pure $ map userDisplayToUserFrontDisplay 
                $ filter (\User{..} -> token == t) $ M.elems db

instance ToRowOfT [Page] where
    putToState [p] = modify (\TestState{..} -> TestState{tsPage = p, ..})

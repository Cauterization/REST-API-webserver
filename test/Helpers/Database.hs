{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Helpers.Database where

import App.Types

import Control.Monad.Catch
import Control.Monad.State
import Control.Lens qualified as Lens
import Control.Lens ((.~), (%~), (?~))
import Data.Coerce
import Data.Data
import Data.List.Extra
import Data.IntMap qualified as IM
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
import Helpers.Internal

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
    type ToRowOf       TestDB q = (TestEntity q, TestEntity (ToDisplay q))
    type FromRowOf     TestDB r = (TestEntity r)
    type ConnectionOf  TestDB   = ()
    type DatabaseMonad TestDB   = TestMonad

    runMigrations _ _ = pure ()

    mkConnectionIO _ = pure ()

    postToDatabase :: forall e.
        ( ToRowOf TestDB e
        ) => () -> TestQuery -> e -> TestMonad (ID e)
    postToDatabase _ _ eCreate = do
        db <- getTestDatabase @e 
        when (alreadyExists eCreate db) $ throwM $ AlreadyExists ""
        let len = length $ IM.keys db
        putDatabase $ IM.insert len (toDisplay eCreate) db
        pure $ ID len

    getFromDatabase :: forall q r. (ToRowOf TestDB q, FromRowOf TestDB r)
        => () -> TestQuery -> q -> TestMonad [r]
    getFromDatabase _ _ q = do
        putToTestState q
        fs <- gets _tsGetFilters
        applyFilters fs <$> getFromTestDatabase

    putIntoDatabase :: ToRowOf TestDB q => 
        () -> TestQuery-> q -> TestMonad Integer
    putIntoDatabase _ _ q = putIntoTestDatabase q

    deleteFromDatabase :: forall e. TestEntity [ID (e Delete)] => 
        () -> TestQuery -> [ID (e Delete)] -> TestMonad Integer
    deleteFromDatabase _ _ = deleteEntityWithThisIDs @[ID (e Delete)]

getManyOrSingle :: forall e a. 
    (TestEntity (e a), TestEntity (e Display)
    , ToDisplay (e a) ~ e Display, ToDisplay (e Display) ~ e Display
    , Typeable e
    ) => TestMonad [Entity e a]
getManyOrSingle = do
    db <- getDatabase @e
    gets _tsIDs >>= \case
        [] -> pure $ map (\(eID, e) -> Entity (ID eID) $ fromDisplay e) $ IM.toList db
        [eID] -> pure <$> lookupT eID db
        x     -> error $ "getManyOrSingle" <> show x

lookupT :: forall e a. (Typeable e, TestEntity (e a), ToDisplay (e a) ~ e Display
    ) => Int -> IM.IntMap (e Display) -> TestMonad (Entity e a)
lookupT eID = maybe 
    (throwM $ EntityNotFound $ nameOf @e) 
    (pure . Entity (ID eID) . fromDisplay) 
    . IM.lookup eID

class TestUpdate e where
    testUpdate :: e -> ToDisplay e -> ToDisplay e

infixl 4 >/

(>/) :: TestUpdate e => e -> ToDisplay e -> ToDisplay e
(>/) = testUpdate

putEntityIntoTestDatabase :: forall e a. 
    ( TestEntity (Entity e a)
    , TestEntity (e Display)
    , ToDisplay (e Display) ~ e Display
    , ToDisplay (e a) ~ e Display
    , TestUpdate (e a)
    ) => Entity e a -> TestMonad Integer
putEntityIntoTestDatabase Entity{entityID = ID eID, entity = e} = do
    db <- getDatabase @e 
    case IM.lookup eID db of
        Nothing -> pure 0
        Just e' -> putDatabase (IM.insert eID (e >/ e') db) >> pure 1
        

instance {-# OVERLAPPABLE #-} TestEntity [ID a] where
    putToTestState eIDs = modify $ tsIDs %~ (<> map idVal eIDs)

type family DBOf e where
    DBOf e            = EMap (ToDisplay e)

type family ToDisplay q where
    ToDisplay (User     a)        = User     Display
    ToDisplay (Author   a)        = Author   Display
    ToDisplay (Tag      a)        = Tag      Display
    ToDisplay (Category a)        = Category Display
    ToDisplay (Entity e a)        = e        Display
    ToDisplay [EntityFilterParam] = [EntityFilterParam]
    ToDisplay [Token]             = [Token]
    ToDisplay [ID a]              = [ID a]
    ToDisplay (ID a)              = ID a

type family ToFrontCreate q where
    ToFrontCreate (e a) = e (Front Create)

class TestEntity r where
    putToTestState :: r -> TestMonad () 
    getTestDatabase :: TestMonad (DBOf r)
    extractTestDatabaseFromTestState :: TestState -> DBOf r
    alreadyExists :: r -> DBOf r -> Bool
    putDatabase :: EMap r -> TestMonad ()
    withTestDatabase :: DBOf r -> StateMod
    toDisplay :: r -> ToDisplay r
    fromDisplay :: ToDisplay r -> r
    toFrontCreate :: r -> ToFrontCreate r
    getFromTestDatabase :: TestMonad [r]
    applyFilters :: [EntityFilterParam] -> [r] -> [r]
    default applyFilters :: [EntityFilterParam] -> [r] -> [r]
    applyFilters = \case
        [EFPInt limit, EFPInt offset] -> take limit . drop offset
        []                            -> id
    putIntoTestDatabase :: r -> TestMonad Integer
    deleteEntityWithThisIDs :: r -> TestMonad Integer

withDatabase :: forall e. TestEntity (e Display) => DBOf (e Display) -> StateMod
withDatabase = withTestDatabase @(e Display)

getDatabase :: forall e. TestEntity (e Display) => TestMonad (DBOf (e Display))
getDatabase = gets (extractTestDatabaseFromTestState @(e Display))

instance TestEntity [EntityFilterParam] where
    putToTestState = modify . (tsGetFilters .~)

-- | auth user login or user token
instance TestEntity [Text] where
    putToTestState [t] = do
        modify $ tsUserLogin .~ t
        modify $ tsToken ?~ t

instance TestEntity (ID a) where
    putToTestState (ID i) = modify $ tsIDs %~ (i:)

instance TestEntity [ID (User Delete)] where
    deleteEntityWithThisIDs [ID uID] = do
        keys <- IM.keys <$> gets _tsUserDB
        modify $ tsUserDB %~ IM.delete uID
        pure $ if uID `elem` keys then 1 else 0

instance TestEntity [ID (Author Delete)] where
    deleteEntityWithThisIDs [ID uID] = do
        keys <- IM.keys <$> gets _tsAuthorDB
        modify $ tsAuthorDB %~ IM.delete uID
        pure $ if uID `elem` keys then 1 else 0

instance TestEntity [ID (Tag Delete)] where
    deleteEntityWithThisIDs [ID uID] = do
        keys <- IM.keys <$> gets _tsTagDB
        modify $ tsTagDB %~ IM.delete uID
        pure $ if uID `elem` keys then 1 else 0

instance TestEntity [ID (Category Delete)] where
    deleteEntityWithThisIDs [ID uID] = do
        keys <- IM.keys <$> gets _tsCatDB
        modify $ tsCatDB %~ IM.delete uID
        pure $ if uID `elem` keys then 1 else 0
-- instance 
--     postToDatabase :: forall e. 
--         ( ToRowOf TestDB e
--         , FromRowOf TestDB (ID e
--         )
--         ) => () -> TestQuery -> e -> DatabaseMonad TestDB (ID e)
--     postToDatabase _ _ e = do
--         db <- getsDatabase @e
--         when (alreadyExists e db) $ throwM $ AlreadyExists ""
--         let eID = ID $ length (M.toList db) + 1
--         eDisplay <- toDisplay e
--         modify $ insertIntoTestDB @e (coerce eID) eDisplay 
--         pure eID

--     putIntoDatabase :: forall q. ToRowOf TestDB q 
--         => () -> TestQuery -> q -> TestMonad ()
--     putIntoDatabase _ _ q = putEntityToTestDatabase @q q

--     getFromDatabase ::  forall q r. (ToRowOf TestDB q, FromRowOf TestDB r)
--         => () -> TestQuery -> q -> TestMonad [r]
--     getFromDatabase _ _ q = do
--         putToState q
--         getEntityFromTestDatabase @r q

--     deleteFromDatabase :: forall q. ToRowOf TestDB q 
--         => () -> TestQuery -> q -> TestMonad Integer
--     deleteFromDatabase _ _ q = do
--         putToState q
--         deleteEntityFromTestDatabase @q q

-- class ToRowOfT q where
--     alreadyExists    :: q -> DBOf q -> Bool
--     toDisplay        :: q -> TestMonad (ToDisplay q)
--     fromDisplay      :: ToDisplay q -> q
--     getsDatabase     :: TestMonad (DBOf q)
--     insertIntoTestDB :: ID (ToDisplay q) -> ToDisplay q -> StateMod
--     putToState       :: q -> TestMonad ()
--     deleteEntityFromTestDatabase :: q -> TestMonad Integer
--     putEntityToTestDatabase :: q -> TestMonad ()
    
-- class FromRowOfT r where
--     getEntityFromTestDatabase :: q -> TestMonad [r] 

-- type family DBOf e where
--     DBOf e = EMap (ToDisplay e)

-- type family ToDisplay q where
--     ToDisplay (User     a) = User   Display
--     ToDisplay (Author   a) = Author Display
--     ToDisplay (Tag      a) = Tag    Display
--     ToDisplay (Category a) = Category Display

-- instance ToRowOfT [ID (Path Current)] where
--     putToState ids' = modify (\TestState{..} 
--         -> TestState{ids = ids ++ map idVal ids', ..})
--     deleteEntityFromTestDatabase [eID] = deleteAllEntitiesWithID eID

-- instance ToRowOfT [Text] where
--     putToState [t] =  do
--         modify (\TestState{..} -> TestState{tsToken = Just t, ..})
--         modify (\TestState{..} -> TestState{tsUserLogin = Just t, ..})

--     -- | User auth token update
--     putEntityToTestDatabase [newToken] = do
--         db <- gets userDB
--         mbLogin <- gets tsUserLogin
--         case mbLogin of
--             Nothing -> throwM $ EntityNotFound ""
--             Just userLogin -> case filter ((== userLogin) . login . snd) $ M.toList db of
--                 [(uID, User{..})] -> let db' = M.insert uID User{token = newToken, ..} db
--                     in modify (\TestState{..} -> TestState{userDB= db', ..})
--             _ -> throwM $ EntityNotFound ""


-- instance ToRowOfT [Page] where
--     putToState [p] = modify (\TestState{..} -> TestState{tsPage = p, ..})

newtype TestQuery = TestQuery () deriving Show

instance IsString TestQuery where
    fromString _ = TestQuery ()

instance Semigroup TestQuery where
    (<>) _ _ = TestQuery ()

instance Monoid TestQuery where
    mempty = TestQuery ()


-- instance (Show (e Create), Data (e Create), Typeable e, ToRowOfT (e Create)) 
--     => PostableTo TestDB e where
-- instance (Show (e a), Typeable e, Data (e a), FromRowOfT (e a)) 
--     => GettableFrom TestDB e a where
-- instance PuttableTo TestDB e where


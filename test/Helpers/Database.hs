{-# LANGUAGE InstanceSigs #-}
module Helpers.Database where

import App.Types

import Control.Monad.Catch
import Control.Monad.State

import Data.Coerce
import Data.List.Extra
import Data.Map qualified as M
import Data.String
import Database.Database

import Entity.Author
import Entity.User
import Entity.Internal
import Extended.Text (Text)



import HKD.HKD

import Helpers.Monad

import Test.Hspec
import Test.QuickCheck

import Unsafe.Coerce

type StateMod = TestState -> TestState

withDB :: TestDB -> StateMod 
withDB db TState{..} = TState{tDB = db, ..}

instance IsDatabase TestDB  where

    type QueryOf       TestDB   = TestQuery
    type ToRowOf       TestDB q = (Show q, ToRowOfT q)
    type FromRowOf     TestDB r = (Show r, FromRowOfT r)
    type ConnectionOf  TestDB   = ()
    type DatabaseMonad TestDB   = TestMonad

    runMigrations _ _ = pure ()

    mkConnectionIO _ = pure ()

    postToDatabase :: forall e. (ToRowOf TestDB e, FromRowOf TestDB (ID e)
        ) => () -> TestQuery -> e -> DatabaseMonad TestDB (ID e)
    postToDatabase _ _ e = do
        db <- gets tDB
        when (alreadyExists e db) $ throwM $ AlreadyExists ""
        let eID = 1 + length(M.toList db)
        modify (\TState{..} -> TState{tDB = M.insert (toIDSum @(ID e) (ID eID)) (toESum e) db, ..})
        pure (ID eID)

    getFromDatabase _ _ q = fromRowTGet q

class ToRowOfT q where
    toRowT        :: q -> TestMonad EntitySum
    toESum        :: q -> EntitySum
    fromESum      :: EntitySum -> q
    alreadyExists :: q -> TestDB -> Bool
    
class FromRowOfT r where
    fromRowT :: EntitySum -> TestMonad [r]
    fromRowTGet :: q -> TestMonad [r]
    toIDSum :: ID q -> IDSum
    fromIDSum :: IDSum -> ID q

instance ToRowOfT [ID (Path Current)] where
    toRowT = undefined

instance ToRowOfT [Page] where
    toRowT = undefined

instance ToRowOfT (Maybe NotUpdated) where
    toRowT = undefined

instance ToRowOfT (Maybe Text) where
    toRowT = undefined

instance ToRowOfT (Author Create) where
    toRowT = pure . AuthorT 
    toESum = AuthorT
    fromESum (AuthorT a) = a
    alreadyExists a db = notNull $ 
        filter (\(AuthorT a') -> user a' == user a) . 
        filter isAuthor $ 
        M.elems db

isAuthor :: EntitySum -> Bool
isAuthor = \case
    AuthorT{} -> True
    _         -> False

instance FromRowOfT (ID (Author Create)) where
    fromRowTGet q = let AuthorTID aID = unsafeCoerce q in pure [aID]
    toIDSum = AuthorTID . coerce

instance ToRowOfT (User Create) where
    toRowT = pure . UserT 
    toESum = UserT
    fromESum (UserT u) = u
    alreadyExists u db = notNull $ 
        filter (\(UserT u') -> login u' == login u) . 
        filter isUser $ 
        M.elems db

isUser :: EntitySum -> Bool
isUser = \case
    UserT{} -> True
    _       -> False

instance FromRowOfT (ID (User Create)) where
    fromRowTGet q = let UserTID uID = unsafeCoerce q in pure [uID]
    toIDSum = UserTID . coerce

newtype TestQuery = TestQuery () deriving Show

instance IsString TestQuery where
    fromString _ = TestQuery ()

instance Semigroup TestQuery where
    (<>) _ _ = TestQuery ()

instance Monoid TestQuery where
    mempty = TestQuery ()



{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Helpers.Monad where

import App.Internal
import App.Types

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Extra (whenM)
import Control.Monad.State hiding (get)
import Control.Monad.State qualified as State
import Control.Monad.Writer 
import Control.Lens

import Data.IntMap qualified as IM
import Database.Database
import Entity.Author
import Entity.Category
import Entity.Tag
import Entity.User
import Entity.Internal

import Extended.Text (Text)

import Helpers.Author
import Helpers.Category
import Helpers.Tag
import Helpers.User
import Helpers.Internal

import HKD.HKD

import Logger qualified
import Test.QuickCheck
import Data.Coerce
import Data.Maybe

newtype TestMonad a = TestMonad
    { unTestM :: ExceptT AppError 
                (WriterT [(Logger.Verbosity, Text)]
                (State TestState)) a } 
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadWriter [(Logger.Verbosity, Text)] 
        , MonadError AppError
        , MonadState TestState 
        )

instance {-# OVERLAPPABLE #-} Monad m => Impure (AppT m) where
    getCurrentDate = pure testDate
    genToken = pure testToken

instance MonadThrow TestMonad where
    throwM e = case fromException (SomeException e) of
        Just err -> throwError err
        _        -> case fromException (SomeException e) of
            Just err -> throwError $ fromDBException err
            _ -> error $ show e

instance MonadCatch TestMonad where
    catch ma f = ma

instance MonadCatch (AppT TestMonad) where
    catch ma f = ma

instance Logger.RunLogger TestMonad where
    runLogger _ = undefined    

type EMap e = IM.IntMap e

type TDB e =  EMap (e Display)

data TestState = TestState
    { _tsUserDB           :: TDB User
    , _tsAuthorDB         :: TDB Author
    , _tsTagDB            :: TDB Tag
    , _tsCatDB            :: TDB Category
    -- , tagDB            :: EMap (Tag      Display)
    -- , catDB            :: EMap (Category Display)
    , _tsIDs              :: [Int]
    , _tsGetFilters       :: [EntityFilterParam]
    -- , tsPage           :: Int
    -- , tsPaginationSize :: Int
    , _tsToken            :: Maybe Token
    , _tsUserLogin        :: Text
    -- , tsUserPass       :: Maybe Text
    } deriving Show
makeLenses ''TestState

initialState :: TestState
initialState = TestState
    { _tsUserDB           = IM.empty
    , _tsAuthorDB         = IM.empty
    , _tsTagDB            = IM.empty
    , _tsCatDB            = IM.empty
    , _tsIDs              = []
    , _tsGetFilters       = []
    -- , authorDB         = M.empty
    -- , tagDB            = M.empty
    -- , catDB            = M.empty
    -- , ids              = []
    -- , tsPage           = 1
    -- , tsPaginationSize = testPaginationConstant
    , _tsToken            = Nothing
    -- , tsUserLogin      = Nothing
    -- , tsUserPass       = Nothing
    }

-- deleteAllEntitiesWithID :: ID (Path Current) -> TestMonad Integer
-- deleteAllEntitiesWithID eID = do
--     TestState{..} <- State.get
--     let u = maybe 0 (const 1) $ M.lookup (coerce eID) userDB
--         a = maybe 0 (const 1) $ M.lookup (coerce eID) authorDB
--         t = maybe 0 (const 1) $ M.lookup (coerce eID) tagDB
--         c = maybe 0 (const 1) $ M.lookup (coerce eID) catDB
--     put $ TestState
--         { userDB   = M.delete (coerce eID) userDB
--         , authorDB = M.delete (coerce eID) authorDB
--         , tagDB    = M.delete (coerce eID) tagDB
--         , catDB    = M.delete (coerce eID) catDB
--         , ..
--         }
--     pure $ u + a + t + c

instance MonadFail TestMonad where
    fail = error
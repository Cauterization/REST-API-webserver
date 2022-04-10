{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Helpers.Monad where

import App.Internal
import App.Types

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Extra (whenM)
import Control.Monad.State hiding (get)
import Control.Monad.State qualified as State
import Control.Monad.Writer 

import Data.Map qualified as M

import Entity.Author
import Entity.User
import Entity.Internal

import Extended.Text (Text)

import Helpers.Author
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

type EMap e = M.Map (ID e) e

type TDB e = M.Map (ID (e Display)) (e Display)

data TestState = TestState
    { userDB           :: EMap (User   Display)
    , authorDB         :: EMap (Author Display)
    , ids              :: [Int]
    , tsPage           :: Int
    , tsPaginationSize :: Int
    , tsToken          :: Maybe Token
    } deriving Show

initialState :: TestState
initialState = TestState
    { userDB           = M.empty
    , authorDB         = M.empty
    , ids              = []
    , tsPage           = 1
    , tsPaginationSize = testPaginationConstant
    , tsToken          = Nothing
    }

deleteAllEntitiesWithID :: ID (Path Current) -> TestMonad Integer
deleteAllEntitiesWithID eID = do
    TestState{..} <- State.get
    let u = maybe 0 (const 1) $ M.lookup (coerce eID) userDB
        a = maybe 0 (const 1) $ M.lookup (coerce eID) authorDB
    put $ TestState
        { userDB   = M.delete (coerce eID) userDB
        , authorDB = M.delete (coerce eID) authorDB
        , ..
        }
    pure $ u + a


{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Helpers.Monad where

import App.Internal
import App.Types

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Extra (whenM)
import Control.Monad.State hiding (get)
import Control.Monad.Writer 

import Data.Map qualified as M

import Entity.Author
import Entity.User

import Extended.Text (Text)

import Helpers.Author
import Helpers.User

import HKD.HKD

import Logger qualified

import Test.QuickCheck

newtype TestMonad a = TestMonad
    { unTestM :: ExceptT AppError 
                (WriterT [(Logger.Verbosity, Text)]
                (State TestState)) a} 
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

data TestState = TState
    { tDB   :: TestDB
    , tPage :: Page
    } deriving Show

initialState :: TestState
initialState = TState
    { tDB    = M.empty
    , tPage  = 1
    }

type TestDB = M.Map IDSum EntitySum

data IDSum 
    = UserTID (ID (User Create)) 
    | AuthorTID (ID (Author Create))
    deriving (Eq, Ord, Show)

instance Arbitrary IDSum where
    arbitrary = choose @Int (1,2) >>= \case
        1 -> UserTID <$> arbitrary 
        2 -> AuthorTID <$> arbitrary 

data EntitySum 
    = UserT (User Create) 
    | AuthorT (Author Create)
    deriving (Show, Eq, Ord)

instance Arbitrary EntitySum where
    arbitrary = choose @Int (1,2) >>= \case
        1 -> UserT   <$> arbitrary 
        2 -> AuthorT <$> arbitrary 
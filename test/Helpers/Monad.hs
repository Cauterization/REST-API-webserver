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
import Entity.Internal

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

type EMap e = M.Map (ID (e)) (e)

data TestState = TestState
    { userDB   :: EMap (User   Display)
    , authorDB :: EMap (Author Display)
    } deriving Show

initialState :: TestState
initialState = TestState
    { userDB = M.empty
    , authorDB = M.empty
    }

-- instance Arbitrary TestState where
--     arbitrary = do
--         userDB   <- arbitrary
--         n        <- chooseInt (1, 30)
--         authorDB <- fromUserDB n userDB
--         pure TestState{..}

fromUserDB :: Int -> EMap (User Display) -> Gen (EMap (Author Display))    
fromUserDB n user = M.fromList . zip [1..] <$> forM (take n $ M.toList user) genAuthorFromUser 

genAuthorFromUser :: (ID (User Display), User Display) -> Gen (Author Display)
genAuthorFromUser (uID, u) = do
    let user = Entity uID u
    description <- arbitrary
    pure Author{..}
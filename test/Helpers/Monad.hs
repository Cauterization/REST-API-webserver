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
import Entity.Article
import Entity.Draft
import Entity.Category
import Entity.Picture
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
    , _tsArticleDB        :: TDB Article
    , _tsDraftDB          :: TDB Draft
    , _tsPictureDB        :: TDB Picture
    , _tsIDs              :: [Int]
    , _tsGetFilters       :: [EntityFilterParam]
    , _tsToken            :: Maybe Token
    , _tsUserLogin        :: Text
    } deriving Show
makeLenses ''TestState

initialState :: TestState
initialState = TestState
    { _tsUserDB           = IM.empty
    , _tsAuthorDB         = IM.empty
    , _tsTagDB            = IM.empty
    , _tsCatDB            = IM.empty
    , _tsArticleDB        = IM.empty
    , _tsDraftDB          = IM.empty
    , _tsPictureDB        = IM.empty
    , _tsIDs              = []
    , _tsGetFilters       = []
    , _tsToken            = Nothing
    }

instance MonadFail TestMonad where
    fail = error
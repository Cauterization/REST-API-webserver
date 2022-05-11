{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Helpers.Monad where

import App.Internal ( AppError, fromDBException, AppT, Impure(..) )
import App.Types ( Token )
import Control.Lens ( makeLenses )
import Control.Monad.Catch
    ( Exception(fromException),
      SomeException(SomeException),
      MonadThrow(..),
      MonadCatch(..) )
import Control.Monad.Except ( ExceptT(..), MonadError(throwError) )
import Control.Monad.Extra (whenM)
import Control.Monad.State ( StateT(StateT), MonadState, State )
import Control.Monad.State qualified as State
import Control.Monad.Writer ( WriterT(WriterT), MonadWriter )
import Data.IntMap qualified as IM
import Database.Database ( EntityFilterParam )
import Entity.Article ( Article )
import Entity.Author ( Author )
import Entity.Category ( Category )
import Entity.Draft ( Draft )
import Entity.Picture ( Picture )
import Entity.Tag ( Tag )
import Entity.User ( User )
import Extended.Text (Text)
import HKD.HKD ( Display )
import Helpers.Internal ( testToken, testDate )
import Logger qualified

newtype TestMonad a = TestMonad
  { unTestM ::
      ExceptT
        AppError
        ( WriterT
            [(Logger.Verbosity, Text)]
            (State TestState)
        )
        a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadWriter [(Logger.Verbosity, Text)],
      MonadError AppError,
      MonadState TestState
    )

instance {-# OVERLAPPABLE #-} Monad m => Impure (AppT m) where
  getCurrentDate = pure testDate
  genToken = pure testToken

instance MonadThrow TestMonad where
  throwM e = case fromException (SomeException e) of
    Just err -> throwError err
    _ -> case fromException (SomeException e) of
      Just err -> throwError $ fromDBException err
      _ -> error $ show e

instance MonadCatch TestMonad where
  catch ma f = ma

instance MonadCatch (AppT TestMonad) where
  catch ma f = ma

instance Logger.RunLogger TestMonad where
  runLogger _ = undefined

type EMap e = IM.IntMap e

type TDB e = EMap (e Display)

data TestState = TestState
  { _tsUserDB :: TDB User,
    _tsAuthorDB :: TDB Author,
    _tsTagDB :: TDB Tag,
    _tsCatDB :: TDB Category,
    _tsArticleDB :: TDB Article,
    _tsDraftDB :: TDB Draft,
    _tsPictureDB :: TDB Picture,
    _tsIDs :: [Int],
    _tsGetFilters :: [EntityFilterParam],
    _tsToken :: Maybe Token,
    _tsUserLogin :: Text
  }
  deriving (Show)

makeLenses ''TestState

initialState :: TestState
initialState =
  TestState
    { _tsUserDB = IM.empty,
      _tsAuthorDB = IM.empty,
      _tsTagDB = IM.empty,
      _tsCatDB = IM.empty,
      _tsArticleDB = IM.empty,
      _tsDraftDB = IM.empty,
      _tsPictureDB = IM.empty,
      _tsIDs = [],
      _tsGetFilters = [],
      _tsToken = Nothing
    }

instance MonadFail TestMonad where
  fail = error

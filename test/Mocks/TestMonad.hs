{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module Mocks.TestMonad where

import App.AppT (AppT, Env (envLogger))
import App.Error (AppError, fromDBError, handleDBErrors)
import App.Impure (Impure (..))
import App.Types (ID (ID), Token)
import Control.Monad.Catch
  ( Exception (fromException),
    MonadCatch (..),
    MonadThrow (..),
    SomeException (SomeException),
  )
import Control.Monad.Except
  ( ExceptT (..),
    MonadError (throwError),
    MonadTrans (lift),
    join,
  )
import Control.Monad.Reader (asks)
import Control.Monad.State
  ( MonadState (state),
    State,
    StateT (StateT),
    gets,
    modify,
  )
import Control.Monad.Writer (MonadWriter, WriterT (WriterT))
import Database.EntityFilters qualified as Database
import Database.HasDatabase qualified as Database
import Database.Internal qualified as Database
import Entity.Article (Article)
import Entity.Author (Author)
import Entity.Category (Category)
import Entity.Draft (Draft)
import Entity.Internal (Entity)
import Entity.Picture (Picture)
import Entity.Tag (Tag)
import Entity.User (User)
import Extended.Text (Text)
import Extended.Text qualified as T
import HKD.HKD (Display, Front, Update)
import Logger qualified
import Mocks.Constant
  ( testDateConstant,
    testPaginationConstant,
    testTokenConstant,
  )

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

instance Impure (AppT TestMonad) where
  getCurrentDate = pure testDateConstant
  genToken = pure testTokenConstant

instance MonadThrow TestMonad where
  throwM e = case fromException (SomeException e) of
    Just err -> throwError err
    _ -> case fromException (SomeException e) of
      Just err -> throwError (fromDBError err)
      _ -> error $ show e

instance MonadCatch TestMonad where
  catch ma f = ma

instance MonadFail TestMonad where
  fail = error

type TestMonadT = AppT TestMonad

instance MonadState TestState TestMonadT where
  state st = lift (state st)

instance MonadCatch TestMonadT where
  catch ma f = ma

instance Logger.HasLogger TestMonadT where
  mkLog v t = do
    l <- asks envLogger
    lift $ l v t

instance Database.HasDatabase TestMonadT where
  type ConnectionOf TestMonadT = ()

  getDatabaseConnection = pure ()

  type FromRowOf TestMonadT q = TestEntity q

  type ToRowOf TestMonadT q = TestEntity q

  runMigrations _ _ = pure ()

  mkConnectionIO _ = pure ()

  postToDatabase :: forall e. TestEntity e => Database.DBQuery -> e -> TestMonadT (ID e)
  postToDatabase _ _ = postToTestDatabase @e

  getFromDatabase :: forall q e. (TestEntity q, TestEntity e) => Database.DBQuery -> q -> TestMonadT [e]
  getFromDatabase _ = getFromTestDatabase @e

  putIntoDatabase :: forall q. TestEntity q => Database.DBQuery -> q -> TestMonadT Integer
  putIntoDatabase _ _ = putIntoTestDatabase @q

  deleteFromDatabase :: forall e. TestEntity [ID e] => Database.DBQuery -> [ID e] -> TestMonadT Integer
  deleteFromDatabase _ _ = deleteFromTestDatabase @[ID e]

  handleDBErrors = handleDBErrors

class TestEntity a where
  postToTestDatabase :: TestMonadT (ID e)
  postToTestDatabase = ID <$> join (gets postResult)

  getFromTestDatabase :: forall q. TestEntity q => q -> TestMonadT [a]
  getFromTestDatabase q = do
    addToState @q q
    fs <- gets filters
    applyFilters fs <$> getFromState

  addToState :: a -> TestMonadT ()
  addToState _ = pure ()

  getFromState :: TestMonadT [a]
  getFromState = pure []

  applyFilters :: [Database.EntityFilterParam] -> [a] -> [a]
  applyFilters = \case
    [] -> id
    [Database.EFPInt limit, Database.EFPInt offset] -> take (min limit testPaginationConstant) . drop offset
    fs -> error $ show fs

  withGetEntities :: [a] -> StateEndo
  withGetEntities _ = id

  putIntoTestDatabase :: TestMonadT Integer
  putIntoTestDatabase = join $ gets putResult

  deleteFromTestDatabase :: TestMonadT Integer
  deleteFromTestDatabase = join $ gets deleteResult

instance TestEntity [Database.EntityFilterParam] where
  addToState fs = modify (\TestState {..} -> TestState {filters = fs, ..})

instance TestEntity (Entity a Update)

instance TestEntity (ID a)

instance TestEntity [ID a]

data TestState = TestState
  { postResult :: TestMonadT Int,
    putResult :: TestMonadT Integer,
    deleteResult :: TestMonadT Integer,
    filters :: [Database.EntityFilterParam],
    getTags :: TestMonadT [Entity Tag (Front Display)],
    getAuthors :: TestMonadT [Entity Author (Front Display)],
    getUsers :: TestMonadT [Entity User (Front Display)],
    getUsersDisplay :: TestMonadT [Entity User Display],
    getCategories :: TestMonadT [Entity Category (Front Display)],
    getCategoriesID :: TestMonadT [ID (Category (Front Update))],
    getPictures :: TestMonadT [Picture (Front Display)],
    getEntityPictures :: TestMonadT [Entity Picture (Front Display)],
    getArticles :: TestMonadT [Entity Article (Front Display)],
    getDrafts :: TestMonadT [Entity Draft (Front Display)],
    userToken :: Maybe Token
  }

initialState :: TestState
initialState =
  TestState
    { postResult = pure defaultPostResult,
      putResult = pure defaultPutResult,
      deleteResult = pure defaultDeleteResult,
      filters = [],
      getTags = pure [],
      getAuthors = pure [],
      getUsers = pure [],
      getUsersDisplay = pure [],
      getCategories = pure [],
      getCategoriesID = pure [],
      getPictures = pure [],
      getEntityPictures = pure [],
      getArticles = pure [],
      getDrafts = pure [],
      userToken = Nothing
    }

defaultPostResult :: Int
defaultPostResult = 1

defaultPutResult :: Integer
defaultPutResult = 1

defaultDeleteResult :: Integer
defaultDeleteResult = 1

type StateEndo = TestState -> TestState

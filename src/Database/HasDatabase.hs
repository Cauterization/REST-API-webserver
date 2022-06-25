{-# LANGUAGE QuantifiedConstraints #-}

module Database.HasDatabase where

import App.Types (ID)
import Control.Monad.Catch (MonadThrow)
import Data.Kind (Constraint, Type)
import Database.Config (Config)
import Database.EntityFilters (EntityFilterParam)
import Database.Internal (DBQuery)
import Logger qualified

class
  (Monad m, Logger.HasLogger m, MonadThrow m, ToRowOf m [EntityFilterParam]) =>
  HasDatabase (m :: Type -> Type)
  where
  type ConnectionOf m :: Type
  getDatabaseConnection :: m (ConnectionOf m)
  type ToRowOf m q :: Constraint
  type FromRowOf m r :: Constraint
  runMigrations :: Config -> Logger.Logger IO -> IO ()
  mkConnectionIO :: Config -> IO (ConnectionOf m)
  handleDBErrors :: m a -> m a
  postToDatabase ::
    ToRowOf m e =>
    DBQuery ->
    e ->
    m (ID e)
  getFromDatabase ::
    ( ToRowOf m q,
      FromRowOf m r
    ) =>
    DBQuery ->
    q ->
    m [r]
  putIntoDatabase ::
    ToRowOf m q =>
    DBQuery ->
    q ->
    m Integer
  deleteFromDatabase ::
    ToRowOf m [ID e] =>
    DBQuery ->
    [ID e] ->
    m Integer

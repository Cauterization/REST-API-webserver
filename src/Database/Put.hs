module Database.Put where

import App.Types (fieldsOf, nameOf, withPluralEnding)
import Control.Monad (when)
import Control.Monad.Catch (MonadThrow (..))
import Data.Data (Data, Typeable)
import Data.Kind (Type)
import Data.List (intercalate)
import Data.String (IsString (..))
import Database.HasDatabase (HasDatabase (..))
import Database.Internal (DBError (EntityNotFound), DBQuery)
import Entity.Internal (Entity (..))
import Entity.Internal qualified as Entity
import Logger qualified

class Puttable (x :: Type) where
  putQuery :: DBQuery
  default putQuery :: forall e a. (x ~ e a, Typeable e, Data (e a)) => DBQuery
  putQuery =
    mconcat
      [ "UPDATE ",
        fromString $ withPluralEnding $ nameOf @e,
        " SET ",
        toCoalesce $ fieldsOf @(e a),
        " WHERE id = ? "
      ]

toCoalesce :: (IsString a) => [String] -> a
toCoalesce =
  fromString . intercalate ", "
    . map (\s -> s <> " = COALESCE (?, " <> s <> ")")

putEntity ::
  forall e (m :: Type -> Type) a.
  ( HasDatabase m,
    Monad m,
    MonadThrow m,
    Logger.HasLogger m,
    ToRowOf m (Entity e a),
    Puttable (e a),
    Typeable e
  ) =>
  Entity.Entity e a ->
  m ()
putEntity e = do
  let q = putQuery @(e a)
  res <- putIntoDatabase @m @(Entity e a) q e
  when (res == 0) $ throwM $ EntityNotFound $ nameOf @e <> " not found."

putEntityWith ::
  forall x (m :: Type -> Type).
  ( HasDatabase m,
    Monad m,
    MonadThrow m,
    Logger.HasLogger m,
    Puttable x,
    ToRowOf m x
  ) =>
  x ->
  m Integer
putEntityWith x = do
  let q = putQuery @x
  putIntoDatabase @m
    q
    x

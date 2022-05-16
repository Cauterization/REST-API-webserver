module Database.Put where

import App.Types ( fieldsOf, nameOf )
import Control.Monad ( when )
import Control.Monad.Catch ( MonadThrow(..) )
import Data.Data ( Data, Typeable )
import Data.Kind ( Type )
import Data.List (intercalate)
import Data.String ( IsString(..) )
import Database.HasDatabase ( HasDatabase(..) )
import Database.Internal
    ( withPluralEnding,
      DBError(EntityNotFound),
      IsDatabase(putIntoDatabase, ToRowOf),
      QConstraints )
import Entity.Internal (Entity (..))
import Entity.Internal qualified as Entity
import Logger qualified

class Puttable (e :: Type) where
  putQuery :: (IsString s, Monoid s) => s

instance
  {-# OVERLAPPABLE #-}
  ( Typeable e,
    Data (e a)
  ) =>
  Puttable (Entity e a)
  where
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
    ToRowOf (Database m) (Entity e a),
    Puttable (Entity e a),
    QConstraints (Database m),
    Typeable e
  ) =>
  Entity.Entity e a ->
  m ()
putEntity e = do
  connection <- getDatabaseConnection
  let q = putQuery @(Entity e a)
  res <- liftDatabase (putIntoDatabase @(Database m) @(Entity e a) connection q e)
  when (res == 0) $ throwM $ EntityNotFound $ nameOf @e <> " not found."

putEntityWith ::
  forall x (m :: Type -> Type).
  ( HasDatabase m,
    IsDatabase (Database m),
    Monad m,
    MonadThrow m,
    Logger.HasLogger m,
    Puttable x,
    QConstraints (Database m),
    ToRowOf (Database m) x
  ) =>
  x ->
  m Integer
putEntityWith x = do
  connection <- getDatabaseConnection
  let q = putQuery @x
  liftDatabase $
    putIntoDatabase @(Database m)
      connection
      q
      x

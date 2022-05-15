module Database.Delete where

import App.Types ( ID, nameOf )
import Control.Monad (when)
import Control.Monad.Catch ( MonadThrow(..) )
import Data.Data ( Data, Typeable )
import Data.Kind ( Type )
import Data.String ( IsString(..) )
import Database.HasDatabase ( HasDatabase(..) )
import Database.Internal
    ( IsDatabase(deleteFromDatabase, ToRowOf),
      DBError(EntityNotFound),
      QConstraints,
      withPluralEnding )
import HKD.HKD ( Delete )
import Logger qualified

class Deletable (e :: Type -> Type) a where
  deleteQuery :: (IsString s, Monoid s) => s

instance {-# OVERLAPPABLE #-} (Data (e Delete), Typeable e) => Deletable e a where
  deleteQuery =
    mconcat
      [ "DELETE FROM ",
        fromString $ withPluralEnding $ nameOf @e,
        " WHERE id = ?"
      ]

deleteEntity ::
  forall e (m :: Type -> Type).
  ( HasDatabase m,
    IsDatabase (Database m),
    Monad m,
    MonadThrow m,
    Logger.HasLogger m,
    Deletable e Delete,
    ToRowOf (Database m) [ID (e Delete)],
    QConstraints (Database m),
    Typeable e
  ) =>
  [ID (e Delete)] ->
  m ()
deleteEntity e = do
  connection <- getDatabaseConnection
  let q = deleteQuery @e @Delete
  res <- liftDatabase (deleteFromDatabase @(Database m) connection q e)
  when (res == 0) (throwM $ EntityNotFound $ nameOf @e <> " not found.")

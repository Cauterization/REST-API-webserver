module Database.Delete where

import App.Types (ID, nameOf, withPluralEnding)
import Control.Monad (when)
import Control.Monad.Catch (MonadThrow (..))
import Data.Data (Data, Typeable)
import Data.Kind (Type)
import Data.String (IsString (..))
import Database.HasDatabase (HasDatabase (..))
import Database.Internal (DBError (EntityNotFound), DBQuery)
import HKD.Delete (Delete)

class Deletable (e :: Type -> Type) where
  deleteQuery :: DBQuery
  default deleteQuery :: (Data (e Delete), Typeable e) => DBQuery
  deleteQuery = deleteQueryDefault @e

deleteQueryDefault :: forall e. (Data (e Delete), Typeable e) => DBQuery
deleteQueryDefault =
  mconcat
    [ "DELETE FROM ",
      fromString $ withPluralEnding $ nameOf @e,
      " WHERE id = ?"
    ]

deleteEntity ::
  forall e (m :: Type -> Type).
  ( HasDatabase m,
    Deletable e,
    ToRowOf m [ID (e Delete)],
    Typeable e
  ) =>
  [ID (e Delete)] ->
  m ()
deleteEntity e = do
  let q = deleteQuery @e
  res <- deleteFromDatabase @m q e
  when (res == 0) (throwM $ EntityNotFound $ nameOf @e <> " not found.")

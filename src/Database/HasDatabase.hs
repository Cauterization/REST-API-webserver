module Database.HasDatabase where

import Data.Kind (Type)
import Database.Internal

class (IsDatabase (Database m)) => HasDatabase m where
  type Database (m :: Type -> Type) :: Type

  liftDatabase :: DatabaseMonad (Database m) a -> m a

  getDatabaseConnection :: m (ConnectionOf (Database m))


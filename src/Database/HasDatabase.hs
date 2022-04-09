module Database.HasDatabase where

import Control.Monad.IO.Class

import Database.Internal
import Database.Query
import Data.Kind (Type)

import App.Types

class ( IsDatabase (Database m)
      ) 
    => HasDatabase m where

    type family Database (m :: Type -> Type) :: Type

    liftDatabase :: DatabaseMonad (Database m) a -> m a

    getDatabaseConnection :: m (ConnectionOf (Database m))

    getPaginationSize :: m PaginationSize 


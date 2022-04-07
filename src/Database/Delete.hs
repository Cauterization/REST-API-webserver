{-# LANGUAGE UndecidableSuperClasses #-}

module Database.Delete where

import Control.Monad.Catch

import Data.Kind
import Data.Data
import Entity.Internal qualified as Entity

import HKD.HKD

import Database.Internal
import Database.Query
import Data.String
import Database.HasDatabase
import qualified Logger
import App.Types (ID, Path, Current)
import Data.Coerce
import Data.List (intercalate)
import Control.Monad (when)

class DeletableFrom db (e :: Type -> Type) where 

    deleteQuery :: Query db (e Delete)

instance {-# OVERLAPS #-} (Data (e Delete), Typeable e
    , ToRowOf db [ID (Path Current)]
    , QConstraints db)
    => DeletableFrom db e where

    deleteQuery = mconcat
        [ "DELETE FROM " , fromString $ Entity.nameOf @e, "s "
        , "WHERE id = ?"
        ]

deleteEntity :: forall e (m :: Type -> Type) .
    ( HasDatabase m
    , IsDatabase (Database m)
    , Monad m
    , MonadThrow m
    , Logger.HasLogger m
    , DeletableFrom (Database m) e
    , ToRowOf (Database m) [ID (Path Current)]
    , QConstraints (Database m)
    -- , FromRowOf (Database m) (ID (e Delete))
    , Typeable e
    ) => [ID (Path Current)] -> m ()
deleteEntity e = do
    connection <- getDatabaseConnection
    let q = unQuery $ deleteQuery @(Database m) @e
    Logger.sql q
    res <- liftDatabase (deleteFromDatabase @(Database m) connection q e)
    when (res == 0) (throwM $ EntityNotFound  $ Entity.nameOf @e )
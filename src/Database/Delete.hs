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
import App.Types 
import Control.Monad (when)

class Deletable (e :: Type -> Type) a where 

    deleteQuery :: (IsString s, Monoid s) => s

instance {-# OVERLAPPABLE #-} (Data (e Delete), Typeable e) => Deletable e a where

    deleteQuery = mconcat
        [ "DELETE FROM " , fromString $ withPluralEnding $ nameOf @e
        , " WHERE id = ?"
        ]                   

deleteEntity :: forall e (m :: Type -> Type) .
    ( HasDatabase m
    , IsDatabase (Database m)
    , Monad m
    , MonadThrow m
    , Logger.HasLogger m
    , Deletable e Delete
    , ToRowOf (Database m) [ID (e Delete)]
    , QConstraints (Database m)
    , Typeable e
    ) => [ID (e Delete)] -> m ()
deleteEntity e = do
    connection <- getDatabaseConnection
    let q = deleteQuery @e @Delete
    Logger.sql q
    res <- liftDatabase (deleteFromDatabase @(Database m) connection q e)
    when (res == 0) (throwM $ EntityNotFound $ nameOf @e <> " not found.")
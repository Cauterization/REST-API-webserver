{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE TypeApplications #-}

module Database.Database where

import Data.Data
import Data.Kind

import Database.Config


import HKD.Display

import Types 


import qualified Logger.Handle as Logger
import Control.Monad.IO.Class
import Server.Base qualified as Base

type family Database (app :: * -> *) :: *

type family Connection (db :: *) :: *

type family FromRow (db :: *) :: * -> Constraint

class HasPagSize m where
    getPagSize :: m Int

instance Base.HasEnv m => HasPagSize m where
    getPagSize = Base.getConfig $ cPagSize . Base.cDatabase
    
class (Typeable e, Data (e Display), FromRow db (e Display)
      ) => Entity db (e :: * -> *) where
    getE :: forall m id. ( HasDatabase m, Database m ~ db)
        => Connection db -> [ID id] -> Page -> m [e Display]
    getE = getEDefault @m

class IsDatabase (db :: *) where
    mkConnectionIODB :: Config -> IO (Connection db)
    runMigrationsDB :: Logger.Logger IO -> Connection db -> IO ()
    getEDefaultDB :: 
        forall m e id.( Database m ~ db, Entity db e, MonadIO  m, HasPagSize m) 
        => Connection (Database m) -> [ID id] -> Page -> m [e Display] 

class IsDatabase (Database m) => HasDatabase (m :: * -> *) where

    mkConnectionIO :: Config -> IO (Connection (Database m))
    mkConnectionIO = mkConnectionIODB @(Database m)

    getConnection  :: m (Connection (Database m))

    runMigrations :: Logger.Logger IO -> Connection (Database m) -> IO ()
    runMigrations = runMigrationsDB @(Database m)

    getEDefault :: forall e id. Entity (Database m) e
        => Connection (Database m) -> [ID id] -> Page -> m [e Display] 
    default getEDefault :: forall e id. (Entity (Database m) e, MonadIO m, HasPagSize m) 
        => Connection (Database m) -> [ID id] -> Page -> m [e Display] 
    getEDefault = getEDefaultDB

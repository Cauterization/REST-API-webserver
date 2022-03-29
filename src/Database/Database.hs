{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE TypeApplications #-}

module Database.Database where

import Data.Data
import Data.Kind

import Database.Config

import Data.String (IsString)

import Entities.Internal

import HKD.Display
import HKD.Front

import Types 


import qualified Logger.Handle as Logger

import Server.Base qualified as Base


type family Database (app :: * -> *) :: *

type family Connection (db :: *) :: *

type family FromRow (db :: *) :: * -> Constraint

type family Query (db :: *) :: *

class HasPagSize m where
    getPagSize :: m Int

instance Base.HasEnv m => HasPagSize m where
    getPagSize = Base.getConfig $ cPagSize . Base.cDatabase

class HasConnection m where
    getConn :: m (Connection (Database m))

newtype EQuery db e = EQuery {unEQuery :: Query db}

deriving via (Query db) instance Semigroup (Query db) => Semigroup (EQuery db e)
deriving via (Query db) instance Monoid    (Query db) => Monoid (EQuery db e)
deriving via (Query db) instance IsString  (Query db) => IsString (EQuery db e)

-- | Database class for each server entity
class ( Typeable e
    
    , Data (e (Front Display))
    , FromRow db (e (Front Display))

    ) => DBEntity db (e :: * -> *) where
    getE :: forall m id. ( HasDatabase m, Database m ~ db)
        => [ID id] -> Page -> m [e (Front Display)]
    getE = getEDefault @m

    getEQuery :: forall a. (IsDatabase db, Data (e a)) => EQuery db (e a)
    getEQuery = getEQueryDefault @db

-- | Database class for databases
class IsDatabase (db :: *) where
    type DBConstraints db (m :: * -> *) :: Constraint 
    mkConnectionIODB :: Config -> IO (Connection db)
    runMigrationsDB :: Config -> Logger.Logger IO -> IO ()
    getEDefaultDB :: 
        forall m e id. (Database m ~ db, DBEntity db e, MConstraints m, HasPagSize m) 
        => [ID id] -> Page -> m [e (Front Display)] 

    getEQueryDefault :: forall (e :: * -> *) (a :: *). 
        (DBEntity db e, Data (e a))
        => EQuery db (e a)

type MConstraints m = DBConstraints (Database m) m

-- | Database class for server 
class IsDatabase (Database m) => HasDatabase (m :: * -> *) where

    mkConnectionIO :: Config -> IO (Connection (Database m))
    mkConnectionIO = mkConnectionIODB @(Database m)

    runMigrations :: Config -> Logger.Logger IO -> IO ()
    runMigrations = runMigrationsDB @(Database m)

    getEDefault :: forall e id. DBEntity (Database m) e
        => [ID id] -> Page -> m [e (Front Display)] 
    default getEDefault :: forall e id. (DBEntity (Database m) e, MConstraints m
        , HasPagSize m) 
        => [ID id] -> Page -> m [e (Front Display)] 
    getEDefault = getEDefaultDB

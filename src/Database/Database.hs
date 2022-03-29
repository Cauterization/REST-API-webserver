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
    
newtype EQuery db e = EQuery {unEQuery :: Query db}

deriving via (Query db) instance Semigroup (Query db) => Semigroup (EQuery db e)
deriving via (Query db) instance Monoid    (Query db) => Monoid (EQuery db e)
deriving via (Query db) instance IsString  (Query db) => IsString (EQuery db e)

class (Typeable e, Data (e (Front Display)), FromRow db (e (Front Display))
      ) => DBEntity db (e :: * -> *) where
    getE :: forall m id. ( HasDatabase m, Database m ~ db)
        => Connection db -> [ID id] -> Page -> m [e (Front Display)]
    getE = getEDefault @m

    getEQ :: forall a. IsDatabase db => EQuery db (e a)
    getEQ = getEQDefault @db


class IsDatabase (db :: *) where
    type DBConstraints db (m :: * -> *) :: Constraint 
    mkConnectionIODB :: Config -> IO (Connection db)
    runMigrationsDB :: Logger.Logger IO -> Connection db -> IO ()
    getEDefaultDB :: 
        forall m e id.( Database m ~ db, DBEntity db e, MConstraints m, HasPagSize m) 
        => Connection (Database m) -> [ID id] -> Page -> m [e (Front Display)] 

    getEQDefault :: forall (e :: * -> *) (a :: *). DBEntity db e 
        => EQuery db (e a)

type MConstraints m = DBConstraints (Database m) m

class IsDatabase (Database m) => HasDatabase (m :: * -> *) where

    mkConnectionIO :: Config -> IO (Connection (Database m))
    mkConnectionIO = mkConnectionIODB @(Database m)

    getConnection  :: m (Connection (Database m))

    runMigrations :: Logger.Logger IO -> Connection (Database m) -> IO ()
    runMigrations = runMigrationsDB @(Database m)

    getEDefault :: forall e id. DBEntity (Database m) e
        => Connection (Database m) -> [ID id] -> Page -> m [e (Front Display)] 
    default getEDefault :: forall e id. (DBEntity (Database m) e, MConstraints m
        , HasPagSize m) 
        => Connection (Database m) -> [ID id] -> Page -> m [e (Front Display)] 
    getEDefault = getEDefaultDB

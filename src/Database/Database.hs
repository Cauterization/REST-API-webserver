{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Database 
    ( module Database.Config
    , module Database.Query
    , FromRow
    , ToField
    , DBEntity(..)
    , IsDatabase(..)
    , Connection
    , HasPagSize(..)
    , HasConnection(..)
    , MConstraints
    , HasDatabase(..)
    ) where

import Data.Data
import Data.Kind


import Data.Char (toUpper)
import Data.List (intersperse)

import Data.String (IsString)

import Database.Config
import Database.Query

import Entities.Internal
import HKD.Display
import HKD.Front

import Types 


import qualified Logger.Handle as Logger

import Server.Base qualified as Base
import GHC.Exts (IsString(fromString))
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import Control.Arrow (Arrow(first))
import Data.Function ((&))
import Control.Applicative ((<|>))
import Data.Text


type family Connection (db :: *) :: *

type family FromRow (db :: *) :: * -> Constraint

type family ToField (db :: *) :: * -> Constraint

class HasPagSize m where
    getPagSize :: m Int

instance Base.HasEnv m => HasPagSize m where
    getPagSize = Base.getConfig $ cPagSize . Base.cDatabase

class HasConnection m where
    getConn :: m (Connection (Database m))

-- | Database classes for each api entity
class ( Typeable e
    
    , Data (e (Front Display))
    , FromRow db (e (Front Display))

    ) => DBEntity db (e :: * -> *) where

    collection :: Text
    collection = nameE @e

    getE :: forall m id. (HasDatabase m, Database m ~ db)
        => [ID id] -> Page -> m [e (Front Display)]
    getE = getEDefault @m

    getEQuery :: forall a. (IsDatabase db, Data (e a)) => EQuery db (e a)
    getEQuery = getEQueryDefault @db

getQuery :: forall db e a. (IsDatabase db, Data (e a), QConstraints db, DBEntity db e) => Query db
getQuery = unEQuery $ getEQuery @db @e @a
 
-- | Database class for databases
class IsDatabase (db :: *) where
    type DBConstraints db (m :: * -> *) :: Constraint 
    mkConnectionIODB :: Config -> IO (Connection db)
    runMigrationsDB :: Config -> Logger.Logger IO -> IO ()
    getEDefaultDB :: forall m e id. 
        (Database m ~ db, DBEntity db e, MConstraints m, HasPagSize m) 
        => [ID id] -> Page -> m [e (Front Display)] 

    getEByIDDefaultDB :: forall m e id. 
        (Database m ~ db, DBEntity db e, MConstraints m, HasPagSize m, ToField db (ID id)) 
        => [ID id] -> m (e (Front Display))

    getEQueryDefault :: forall (e :: * -> *) (a :: *). 
        (DBEntity db e, Data (e a))
        => EQuery db (e a)

    getEByIDQueryDefault :: forall (e :: * -> *) (a :: *). 
        (DBEntity db e, Data (e a))
        => EQuery db (e a)

type MConstraints m = DBConstraints (Database m) m

-- | Database class for server 
class IsDatabase (Database m) => HasDatabase (m :: * -> *) where

    type family Database m :: *

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

    getEByIDDefault :: forall e id. DBEntity (Database m) e
        => [ID id] -> m (e (Front Display))
    default getEByIDDefault :: forall e id. (DBEntity (Database m) e, MConstraints m
        , HasPagSize m, ToField (Database m) (ID id)) 
        => [ID id] -> m (e (Front Display))
    getEByIDDefault = getEByIDDefaultDB
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
    , GettableFrom(..)
    , GArgs(..)
    , toGArgs
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
import Extended.Text (Text)
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

import Control.Monad.Catch


type family Connection (db :: *) :: *

type family FromRow (db :: *) :: * -> Constraint

type family ToField (db :: *) :: * -> Constraint

class HasPagSize m where
    getPagSize :: m Int

instance Base.HasEnv m => HasPagSize m where
    getPagSize = Base.getConfig $ cPagSize . Base.cDatabase

class HasConnection m where
    getConn :: m (Connection (Database m))

class ( Typeable e
    ) => DBEntity db (e :: * -> *) where

    collection :: Text
    collection = nameE @e

class  ( DBEntity db e
       , FromRow db (e (Front Display))
       ) => GettableFrom db e where

    type family GArgs e :: *

    toGArgs :: (Monad m, MonadThrow m) => [ID Path] -> m (GArgs e)
    default toGArgs :: (GArgs e ~ ID Path, Monad m, MonadThrow m) => [ID Path] -> m (GArgs e)
    toGArgs ids = case ids of
        [i] -> pure i
        _ -> Base.arityError 1 $ length ids

    getFromDB :: forall m. 
        (HasDatabase m, Database m ~ db, MConstraints m, ToField db (GArgs e))
        => (EQuery db (e (Front Display)) -> EQuery db (e (Front Display))) 
        -> GArgs e 
        -> m [e (Front Display)]
    default getFromDB :: forall m. 
        ( HasDatabase m
        , Database m ~ db
        , MConstraints m
        , ToField db (GArgs e)
        , GArgs e ~ Page
        , Data (e (Front Display))
        )
        => (EQuery db (e (Front Display)) -> EQuery db (e (Front Display))) 
        -> GArgs e 
        -> m [e (Front Display)]
    getFromDB = getEFromDBDefault
    
    getQuery :: forall a. (IsDatabase db, Data (e a)) => EQuery db (e a)
    getQuery = getEQueryDefault @db

-- class ( DBEntity db e
--       , FromRow db (e (Front Display))
--       ) => GettableManyFrom db e where

--     getE :: forall m. (HasDatabase m, Database m ~ db, MConstraints m)
--         => Page -> m [e (Front Display)]
--     default getE :: forall m. (HasDatabase m, Database m ~ db, MConstraints m)
--         => Page -> m [e (Front Display)]
--     getE = getEDefault @(Database m)

--     getEQuery :: forall a. (IsDatabase db, Data (e a)) => EQuery db (e a)
--     getEQuery = getEQueryDefault @db

-- class (DBEntity db e, ToField db (ID Path), FromRow db (e (Front Display))) 
--     => GettableSingleFrom db e where

--     getEByID :: forall m. (HasDatabase m, Database m ~ db, MConstraints m)
--         => ApplyArity e -> m (e (Front Display))
--     default getEByID :: forall m. (HasDatabase m, Database m ~ db, MConstraints m
--         , ApplyArity e ~ ID Path)
--         => ApplyArity e -> m (e (Front Display))
--     getEByID = getEByIDDefault @(Database m)

--     getEByIDQuery :: forall a. (IsDatabase db, Data (e a)) => EQuery db (e a)
--     getEByIDQuery = getEQueryDefault @db

-- | Database class for databases
class IsDatabase (db :: *) where
    type DBConstraints db (m :: * -> *) :: Constraint 
    mkConnectionIO :: Config -> IO (Connection db)
    runMigrations :: Config -> Logger.Logger IO -> IO ()

    getEFromDBDefault :: forall m e a.
        ( Database m ~ db
        , HasDatabase m
        , GettableFrom db e
        , FromRow db (e a)
        , ToField db (GArgs e)
        , Data (e a)
        ) =>
        (EQuery db (e a) -> EQuery db (e a)) 
        -> GArgs e 
        -> m [e a]

    getEQueryDefault :: forall (e :: * -> *) (a :: *). 
        (DBEntity db e, Data (e a))
        => EQuery db (e a)

    getEByIDQueryDefault :: forall (e :: * -> *) (a :: *). 
        (DBEntity db e, Data (e a))
        => EQuery db (e a)

type MConstraints m = (DBConstraints (Database m) m, IsDatabase (Database m), Base.HasEnv m)

-- | Database class for server 
class MConstraints m => HasDatabase (m :: * -> *) where

    type family Database m :: *

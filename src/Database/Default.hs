{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE TypeApplications #-}


module Database.Default where

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


type family Constraints' (db :: *) (m :: * -> *) :: Constraint

type Constraints (m :: * -> *) = Constraints' (Database m) m

class Default (db :: *) where
    mkConnectionIO :: Config -> IO (Connection db)
    runMigrations :: Logger.Logger IO -> Connection db -> IO ()
    getE :: 
        forall m e id. (Database m ~ db, HasPagSize m, Constraints m, Monad m) 
        => Connection (Database m) -> [ID id] -> Page -> m [e Display] 
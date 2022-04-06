module Database.Internal where

import Data.Kind (Type, Constraint)

import Database.Config

import Logger qualified

class IsDatabase db where

    type QueryOf db :: Type
    type ToRowOf db q :: Constraint
    type FromRowOf db r :: Constraint
    type ConnectionOf db :: Type
    type DatabaseMonad db :: Type -> Type

    runMigrations :: Config -> Logger.Logger IO -> IO ()

    mkConnectionIO :: Config -> IO (ConnectionOf db)

    getFromDatabase :: 
        ( ToRowOf db q
        , FromRowOf db r
        ) => ConnectionOf db -> QueryOf db -> q -> (DatabaseMonad db) [r]

{-# LANGUAGE UndecidableSuperClasses #-}

module Database.Post where

import Control.Monad.Catch

import Data.Kind
import Data.Data

import HKD.HKD

import Database.Internal
import Database.Query
import Data.String
import Database.HasDatabase
import qualified Logger
import App.Types 
import Data.List (intercalate)

class PostableTo db (e :: Type -> Type) where 

    tableNamePost :: Query db (e Create)
    default tableNamePost :: (Typeable e, QConstraints db) => Query db (e Create)
    tableNamePost =  fromString $ nameOf @e <>  "s"

    postQuery :: Query db (e Create)
    default postQuery :: (Data (e Create), Typeable e
        , ToRowOf db (e Create)
        , QConstraints db) =>  Query db (e Create)
    postQuery = mconcat
        [ "INSERT INTO " , tableNamePost
        , " (",  fieldsQuery @(e Create), ") "
        , "VALUES ("
        , fromString $ intercalate "," $ fieldsOf @(e Create) >> pure "?"
        , ")"
        ]

postEntity :: forall e (m :: Type -> Type).
    ( HasDatabase m
    , IsDatabase (Database m)
    , Monad m
    , MonadThrow m
    , Logger.HasLogger m
    , PostableTo (Database m) e
    , ToRowOf (Database m) (e Create)
    , QConstraints (Database m)
    , FromRowOf (Database m) (ID (e Create))
    ) => e Create -> m (ID (e Create))
postEntity = postEntityWith (<> " RETURNING id") 

postEntityWith :: forall e (m :: Type -> Type) x.
    ( HasDatabase m
    , IsDatabase (Database m)
    , Monad m
    , MonadThrow m
    , Logger.HasLogger m
    , PostableTo (Database m) e
    , ToRowOf (Database m) (e Create)
    , QConstraints (Database m)
    , FromRowOf (Database m) (ID (e Create))
    ) => 
    (QueryOf (Database m) -> QueryOf (Database m)) -> e Create -> m (ID (e Create))
postEntityWith f e = do
    connection <- getDatabaseConnection
    let q = unQuery $ postQuery @(Database m) @e
    Logger.sql q
    liftDatabase (postToDatabase @(Database m)  connection (f q) e)
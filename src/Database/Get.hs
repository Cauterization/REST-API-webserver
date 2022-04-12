{-# LANGUAGE UndecidableSuperClasses #-}

module Database.Get where

import Data.Kind ( Type )
import Database.Internal
import Database.HasDatabase
import Database.Query


import App.Types

import Entity.Internal qualified as Entity
import Data.String
import Data.Data

import qualified Logger
import Control.Monad.Catch

class FromRowOf db (e a) => GettableFrom db (e :: Type -> Type) a where 

    tableNameGet :: Query db (e a)
    default tableNameGet :: (Typeable e, QConstraints db) => Query db (e a)
    tableNameGet =  fromString $ nameOf @e 

    getQuery :: Query db (e a)
    default getQuery :: (QConstraints db, Data (e a), Typeable e) => Query db (e a)
    getQuery = mconcat 
        ["SELECT ", fromString $ fieldsQuery @(e a) 
        , " FROM ", tableNameGet, "s"]

getEntity :: forall e m a. 
    ( HasDatabase m
    , Monad m
    , MonadThrow m
    , Logger.HasLogger m
    , IsDatabase (Database m)
    , GettableFrom (Database m) e  a
    , ToRowOf (Database m) IDs
    , FromRowOf (Database m) (e a)
    , QConstraints (Database m)
    , Data (e a)
    , Typeable e 
    ) => IDs -> m (e a)
getEntity eIDs = getEntitiesWith eIDs (<> " WHERE id = ?") >>= getSingle

getEntities :: forall e a m. 
    ( HasDatabase m
    , Monad m
    , Logger.HasLogger m
    , IsDatabase (Database m)
    , ToRowOf (Database m) [Page]
    , GettableFrom (Database m) e  a
    , QConstraints (Database m)
    , Data (e a)
    , Typeable e 
    ) => 
    Page -> m [e a]
getEntities page = do
    pagination <- fromString . show <$> getPaginationSize
    getEntitiesWith [page] 
        (<> mconcat [" LIMIT ", pagination, " OFFSET ", pagination , " * (? - 1)"])

getEntitiesWith :: forall e a m x. 
    ( HasDatabase m
    , Monad m
    , Logger.HasLogger m
    , IsDatabase (Database m)
    , ToRowOf (Database m) x
    , GettableFrom (Database m) e a
    , FromRowOf (Database m) (e a)
    , QConstraints (Database m)
    , Data (e a)
    , Typeable e 
    ) => 
    x -> (Query (Database m) (e a) -> Query (Database m) (e a)) -> m [e a]
getEntitiesWith a f = do
    connection <- getDatabaseConnection
    let q = unQuery $ f $ getQuery @(Database m) @e 
    Logger.sql q
    liftDatabase $ getFromDatabase @(Database m) 
        connection 
        q
        a

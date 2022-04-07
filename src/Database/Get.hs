{-# LANGUAGE UndecidableSuperClasses #-}

module Database.Get where

import Data.Kind ( Type )
import Database.Internal
import Database.HasDatabase
import Database.Query
import Control.Monad.Identity

import HKD.HKD

import App.Types

import Entity.Internal qualified as Entity
import Data.String
import Data.Data

import qualified Logger
import Control.Monad.Catch

class FromRowOf db (e a) =>
    GettableFrom db (e :: Type -> Type) a where 

    getQuery :: Query db (e a)

instance {-# OVERLAPS #-} (QConstraints db, Data (e a), Typeable e
    , FromRowOf db (e a)) 
    => GettableFrom db e a where

    getQuery = mconcat ["SELECT ", fromString $ Entity.fieldsQuery @(e a) 
            , " FROM ", fromString $ Entity.nameOf @e <> "s"]

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
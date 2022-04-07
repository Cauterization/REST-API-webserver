{-# LANGUAGE UndecidableSuperClasses #-}

module Database.Post where

import Control.Monad.Catch

import Data.Kind
import Data.Data
import Entity.Internal qualified as Entity

import HKD.HKD

import Database.Internal
import Database.Query
import Data.String
import Database.HasDatabase
import qualified Logger
import App.Types (ID)
import Data.Coerce
import Data.List (intercalate)

class PostableTo db (e :: Type -> Type) where 

    postQuery :: Query db (e Create)

instance {-# OVERLAPS #-} (Data (e Create), Typeable e
    , ToRowOf db (e Create)
    , QConstraints db)
    => PostableTo db e where

    postQuery = mconcat
        [ "INSERT INTO " , fromString $ Entity.nameOf @e, "s"
        , " (",  Entity.fieldsQuery @(e Create), ") "
        , "VALUES ("
        , fromString $ intercalate "," $ Entity.fieldsOf @(e Create) >> pure "?"
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
postEntity e = postEntityWith (<> " RETURNING id") e >>= getSingle

postEntityWith :: forall e (m :: Type -> Type) x.
    ( HasDatabase m
    , IsDatabase (Database m)
    , Monad m
    , MonadThrow m
    , Logger.HasLogger m
    , PostableTo (Database m) e
    , ToRowOf (Database m) (e Create)
    , QConstraints (Database m)
    , FromRowOf (Database m) x
    ) => 
    (QueryOf (Database m) -> QueryOf (Database m)) -> e Create -> m [x]
postEntityWith f e = do
    connection <- getDatabaseConnection
    let q = unQuery $ postQuery @(Database m) @e
    Logger.sql q
    liftDatabase (getFromDatabase @(Database m) @(e Create) @x connection (f q) e)
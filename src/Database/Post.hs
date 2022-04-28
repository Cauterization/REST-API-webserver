{-# LANGUAGE UndecidableSuperClasses #-}

module Database.Post where

import Control.Monad.Catch

import Data.Kind
import Data.Data
import Data.Functor

import HKD.HKD

import Database.Internal
import Database.Query
import Data.String
import Database.HasDatabase
import qualified Logger
import App.Types 
import Data.List (intercalate)

class Postable (e :: Type -> Type) a where 

    postQuery :: (IsString s, Monoid s) => s

instance {-# OVERLAPPABLE #-} (Data (e Create), Typeable e) => Postable e a where
    postQuery = mconcat
        [ "INSERT INTO " , fromString $ withPluralEnding $ nameOf @e
        , " (",  fieldsQuery @(e Create), ") "
        , "VALUES "
        , qmarkFields @e @Create
        ]

qmarkFields :: forall e a s. (Data (e a), IsString s) => s
qmarkFields = fromString $ mconcat [ "(", intercalate "," $ fieldsOf @(e a) $> "?", ")"]

postEntity :: forall e (m :: Type -> Type).
    ( HasDatabase m
    , IsDatabase (Database m)
    , Monad m
    , MonadThrow m
    , Logger.HasLogger m
    , Postable e Create
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
    , Postable e Create
    , ToRowOf (Database m) (e Create)
    , QConstraints (Database m)
    , FromRowOf (Database m) (ID (e Create))
    ) => 
    (QueryOf (Database m) -> QueryOf (Database m)) -> e Create -> m (ID (e Create))
postEntityWith f e = do
    connection <- getDatabaseConnection
    let q = postQuery @e @Create
    Logger.sql q
    liftDatabase (postToDatabase @(Database m) connection (f q) e)

publish :: forall e (m :: Type -> Type) a x.
    ( HasDatabase m
    , IsDatabase (Database m)
    , Monad m
    , MonadThrow m
    , Logger.HasLogger m
    , ToRowOf (Database m) (e a)
    , QConstraints (Database m)
    , FromRowOf (Database m) (ID (e a))
    ) => QueryOf (Database m)  -> e a -> m (ID (e a))
publish q e = do
    connection <- getDatabaseConnection
    Logger.sql q
    liftDatabase (postToDatabase @(Database m) connection q e)
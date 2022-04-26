{-# LANGUAGE UndecidableSuperClasses #-}

module Database.Get where

import Data.Kind ( Type )
import Database.Internal
import Database.HasDatabase
import Database.Query

import Extended.Text (Text)
import qualified Extended.Postgres as Postgres


import App.Types
import App.QueryParams
import GHC.Generics (Generic)
import Entity.Internal qualified as Entity
import Entity.Internal (Entity(..))
import Data.String
import Data.List
import Data.Map qualified as M
import Data.Data

import qualified Logger
import Control.Monad.Catch ( MonadThrow )

data EntityFilter 
    = EFString Text
    | EFNum Text
    | EFDate Text
    | EFLimit
    | EFOffset
    deriving (Eq, Ord)

data EntityFilterParam
    = EFPInt          Int
    | EFPTextOptional (Maybe Text)
    | EFPIntOptional  (Maybe Int)
    | EFPDateOptional (Maybe Date)
    deriving Show

instance Postgres.ToField EntityFilterParam where
    toField = \case
        EFPInt          i -> Postgres.toField @Int i
        EFPIntOptional  i -> Postgres.toField @(Maybe Int)  i
        EFPTextOptional t -> Postgres.toField @(Maybe Text) t
        EFPDateOptional d -> Postgres.toField @(Maybe Date) d

class Gettable (e :: Type -> Type) a where 

    getQuery :: (IsString s, Monoid s) => s
    default getQuery :: (Data (e a), Typeable e, IsString s, Monoid s) => s
    getQuery = mconcat 
        ["SELECT ", "id, ", fromString $ fieldsQuery @(e a) 
        , " FROM ", fromString $ withPluralEnding $ nameOf @e
        ]

    entityFilters :: [EntityFilter]
    entityFilters = [EFLimit, EFOffset]

    entityFiltersQuery ::  (IsString s, Monoid s) => s
    entityFiltersQuery = " LIMIT ? OFFSET ? "

getEntityFilters :: forall e a. Gettable e a => [EntityFilter]
getEntityFilters = sort $ entityFilters @e @a  

instance {-# OVERLAPPABLE #-} 
    ( Typeable e
    , Data (e a)
    ) => Gettable (Entity e) a where

    getQuery = mconcat 
        ["SELECT id, ", fromString $ fieldsQuery @(e a) 
        , " FROM ", fromString $ withPluralEnding $ nameOf @e
        ]

getEntitiesGeneric :: forall e a m. 
    ( HasDatabase m
    , Monad m
    , Logger.HasLogger m
    , IsDatabase (Database m)
    , ToRowOf (Database m) [EntityFilterParam]
    , Gettable e a
    , FromRowOf (Database m) (e a)
    , QConstraints (Database m)
    , Data (e a)
    , Typeable e 
    ) => [EntityFilterParam] -> m [e a]
getEntitiesGeneric filterParams = getEntitiesWith filterParams 
    (<> entityFiltersQuery @e @a)

getEntityGeneric :: forall e m a. 
    ( HasDatabase m
    , Monad m
    , MonadThrow m
    , Logger.HasLogger m
    , IsDatabase (Database m)
    , Gettable e a
    , ToRowOf (Database m) [ID (e a)]
    , FromRowOf (Database m) (e a)
    , QConstraints (Database m)
    , Data (e a)
    , Typeable e 
    , (Eq (e a))
    ) => [ID (e a)] -> m (e a)
getEntityGeneric eID = getEntitiesWith eID (<> " WHERE id = ?") >>= getSingle

getEntitiesWith :: forall e a m x. 
    ( HasDatabase m
    , Monad m
    , Logger.HasLogger m
    , IsDatabase (Database m)
    , ToRowOf (Database m) x
    , Gettable e a
    , FromRowOf (Database m) (e a)
    , QConstraints (Database m)
    , Data (e a)
    , Typeable e 
    ) => 
    x -> (Query (Database m) (e a) -> Query (Database m) (e a)) -> m [e a]
getEntitiesWith a f = do
    connection <- getDatabaseConnection
    let q = unQuery $ f $ getQuery @e @a
    Logger.sql q
    liftDatabase $ getFromDatabase @(Database m) 
        connection 
        q
        a

-- getEntity :: forall e m a. 
--     ( HasDatabase m
--     , Monad m
--     , MonadThrow m
--     , Logger.HasLogger m
--     , IsDatabase (Database m)
--     , GettableFrom (Database m) e  a
--     , ToRowOf (Database m) IDs
--     , FromRowOf (Database m) (e a)
--     , QConstraints (Database m)
--     , Data (e a)
--     , Typeable e 
--     ) => IDs -> m (e a)
-- getEntity eIDs = getEntitiesWith eIDs (<> " WHERE id = ?") >>= getSingle

-- getManyEntities :: forall e a m. 
--     ( HasDatabase m
--     , Monad m
--     , Logger.HasLogger m
--     , IsDatabase (Database m)
--     , ToRowOf (Database m) [Page]
--     , GettableFrom (Database m) e a
--     , QConstraints (Database m)
--     , Data (e a)
--     , Typeable e 
--     ) => 
--     Page -> m [e a]
-- getManyEntities = getManyEntitiesWith id

-- getManyEntitiesWith :: forall e a m. 
--     ( HasDatabase m
--     , Monad m
--     , Logger.HasLogger m
--     , IsDatabase (Database m)
--     , ToRowOf (Database m) [Page]
--     , GettableFrom (Database m) e  a
--     , QConstraints (Database m)
--     , Data (e a)
--     , Typeable e 
--     ) => 
--     (Query (Database m) (e a) -> Query (Database m) (e a)) -> Page -> m [e a]
-- getManyEntitiesWith f page = do
--     pagination <- fromString . show <$> getPaginationSize
--     getEntitiesWith [page] 
--         ((<> mconcat [" LIMIT ", pagination, " OFFSET ", pagination , " * (? - 1)"]) . f)



{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}

module Database.Get where

import App.Types
import Control.Monad.Catch (MonadThrow)
import Data.Data
import Data.Kind (Type)
import Data.List
import Data.String
import Database.HasDatabase
import Database.Internal
import Entity.Internal (Entity (..))
import Extended.Postgres qualified as Postgres
import Extended.Text (Text)
import Logger qualified

data EntityFilter
  = EFString Text
  | EFNum Text
  | EFNumList Text
  | EFDate Text
  | EFLimit
  | EFOffset
  deriving (Eq)

instance Ord EntityFilter where
  compare EFOffset _ = GT
  compare _ EFOffset = LT
  compare EFLimit _ = GT
  compare _ EFLimit = LT
  compare _ _ = EQ

data EntityFilterParam
  = EFPInt Int
  | EFPText Text
  | EFPTextOptional (Maybe Text)
  | EFPIntOptional (Maybe Int)
  | EFPIntListOptional (Maybe [Int])
  | EFPDateOptional (Maybe Date)
  deriving (Show)

instance Postgres.ToField EntityFilterParam where
  toField = \case
    EFPInt i -> Postgres.toField i
    EFPText t -> Postgres.toField t
    EFPIntOptional i -> Postgres.toField i
    EFPIntListOptional il -> Postgres.toField $ Postgres.PGArray <$> il
    EFPTextOptional t -> Postgres.toField t
    EFPDateOptional d -> Postgres.toField d

class Gettable (e :: Type -> Type) a where
  getQuery :: (IsString s, Monoid s) => s
  default getQuery :: (Data (e a), Typeable e, IsString s, Monoid s) => s
  getQuery =
    mconcat
      [ "SELECT ",
        "id, ",
        fromString $ fieldsQuery @(e a),
        " FROM ",
        fromString $ withPluralEnding $ nameOf @e
      ]

  entityFilters :: [EntityFilter]
  entityFilters = defaultFilters

  entityFiltersQuery :: (IsString s, Monoid s) => s
  entityFiltersQuery = " LIMIT ? OFFSET ? "

defaultFilters :: [EntityFilter]
defaultFilters = [EFLimit, EFOffset]

getEntityFilters :: forall e a. Gettable e a => [EntityFilter]
getEntityFilters = sort $ entityFilters @e @a

instance
  {-# OVERLAPPABLE #-}
  ( Typeable e,
    Data (e a)
  ) =>
  Gettable (Entity e) a
  where
  getQuery =
    mconcat
      [ "SELECT id, ",
        fromString $ fieldsQuery @(e a),
        " FROM ",
        fromString $ withPluralEnding $ nameOf @e
      ]

getEntitiesGeneric ::
  forall e a m.
  ( HasDatabase m,
    Monad m,
    Logger.HasLogger m,
    IsDatabase (Database m),
    ToRowOf (Database m) [EntityFilterParam],
    Gettable e a,
    FromRowOf (Database m) (e a),
    QConstraints (Database m),
    Data (e a),
    Typeable e
  ) =>
  [EntityFilterParam] ->
  m [e a]
getEntitiesGeneric filterParams =
  getEntitiesWith
    filterParams
    (<> entityFiltersQuery @e @a)

getEntityGeneric ::
  forall e m a.
  ( HasDatabase m,
    Monad m,
    MonadThrow m,
    Logger.HasLogger m,
    IsDatabase (Database m),
    Gettable e a,
    ToRowOf (Database m) [ID (e a)],
    FromRowOf (Database m) (e a),
    QConstraints (Database m),
    Data (e a),
    Typeable e,
    (Eq (e a))
  ) =>
  [ID (e a)] ->
  m (e a)
getEntityGeneric eID = getEntitiesWith eID (addWhere @(Database m) "id = ?") >>= getSingle

getEntitiesWith ::
  forall e a x m.
  ( HasDatabase m,
    Monad m,
    Logger.HasLogger m,
    IsDatabase (Database m),
    ToRowOf (Database m) x,
    Gettable e a,
    FromRowOf (Database m) (e a),
    QConstraints (Database m),
    Data (e a),
    Typeable e
  ) =>
  x ->
  (QueryOf (Database m) -> QueryOf (Database m)) ->
  m [e a]
getEntitiesWith a f = do
  connection <- getDatabaseConnection
  let q = f $ getQuery @e @a
  -- Logger.sql q
  liftDatabase $
    getFromDatabase @(Database m)
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

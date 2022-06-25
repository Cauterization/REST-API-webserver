module Database.Get where

import App.Types (ID, fieldsQuery, nameOf, withPluralEnding)
import Control.Monad.Catch (MonadThrow)
import Data.Data (Data, Typeable)
import Data.Kind (Type)
import Data.List (sort)
import Data.String (IsString (fromString))
import Database.EntityFilters
  ( EntityFilter (EFLimit, EFOffset),
    EntityFilterParam,
  )
import Database.HasDatabase
  ( HasDatabase (FromRowOf, ToRowOf, getFromDatabase),
  )
import Database.Internal (DBQuery, addWhere, getSingle)
import Entity.Internal (Entity (..))
import Logger qualified

class Gettable (x :: Type -> Type) a where
  getQuery :: DBQuery
  default getQuery :: forall e. (x ~ Entity e, Data (e a), Typeable e) => DBQuery
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

  entityFiltersQuery :: DBQuery
  entityFiltersQuery = " LIMIT ? OFFSET ? "

defaultFilters :: [EntityFilter]
defaultFilters = [EFLimit, EFOffset]

getEntityFilters :: forall e a. Gettable e a => [EntityFilter]
getEntityFilters = sort $ entityFilters @e @a

getEntities ::
  forall e a m.
  ( HasDatabase m,
    Gettable e a,
    FromRowOf m (e a)
  ) =>
  [EntityFilterParam] ->
  m [e a]
getEntities filterParams =
  getEntitiesWith
    filterParams
    (<> entityFiltersQuery @e @a)

getEntity ::
  forall e m a.
  ( HasDatabase m,
    MonadThrow m,
    Logger.HasLogger m,
    Gettable e a,
    ToRowOf m [ID (e a)],
    FromRowOf m (e a),
    Typeable e,
    (Eq (e a))
  ) =>
  [ID (e a)] ->
  m (e a)
getEntity eID = getEntitiesWith eID (addWhere "id = ?") >>= getSingle

getEntitiesWith ::
  forall e a x m.
  ( HasDatabase m,
    Monad m,
    Logger.HasLogger m,
    ToRowOf m x,
    Gettable e a,
    FromRowOf m (e a)
  ) =>
  x ->
  (DBQuery -> DBQuery) ->
  m [e a]
getEntitiesWith a f = do
  let q = f $ getQuery @e @a
  getFromDatabase @m
    q
    a

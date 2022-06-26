module Api.Get where

import App.AppT (Application, Env (envConfig))
import App.Config (Config (..))
import App.Getters
  ( getDateParam,
    getNumListParam,
    getNumParam,
    getParam,
  )
import App.Result (Endpoint)
import App.ResultJSON (ToJSONResult, json)
import App.Router (Router, get)
import App.Types (ID (ID), nameOf)
import Control.Monad (forM)
import Control.Monad.Reader (asks)
import Data.Coerce (coerce)
import Data.Data (Data, Typeable)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Database.Config qualified as Database
import Database.EntityFilters qualified as Database
import Database.Get qualified as Database
import Database.HasDatabase qualified as Database
import Entity.Internal (Entity (..))
import HKD.HKD (Display, Front)
import Logger qualified

type Gettable m e a =
  ( Database.Gettable e a,
    Database.ToRowOf m [ID (e a)],
    Database.FromRowOf m (e a),
    Database.ToRowOf m [Database.EntityFilterParam],
    Data (e a),
    Show (e a),
    Typeable e,
    Eq (e a)
  )

get_ ::
  forall (e :: Type -> Type) (m :: Type -> Type).
  ( Application m,
    Gettable m (Entity e) (Front Display),
    ToJSONResult (Entity e (Front Display))
  ) =>
  Text ->
  Router e m ()
get_ p
  | T.isSuffixOf "{ID}" p = get p (getEntity @(Entity e) @(Front Display))
  | otherwise = get p (getEntities @(Entity e) @(Front Display))

getEntity ::
  forall (e :: Type -> Type) a m.
  ( Application m,
    Gettable m e a,
    ToJSONResult (e a)
  ) =>
  Endpoint m
getEntity eIDs = do
  Logger.info $ "Attempt to get " <> nameOf @e
  entity <- Database.getEntity @e @m @a (map coerce eIDs)
  Logger.info $ nameOf @e <> " was found."
  json @_ entity

getEntities ::
  forall (e :: Type -> Type) a m.
  ( Application m,
    Gettable m e a,
    ToJSONResult (e a)
  ) =>
  Endpoint m
getEntities _ = do
  Logger.info $ "Attempt to get " <> nameOf @e <> "s"
  fs <- getFilters @e @a
  entities <- Database.getEntities @e @a @m fs
  Logger.info $ nameOf @e <> " was found."
  json entities

getFilters ::
  forall e a m.
  ( Application m,
    Gettable m e a
  ) =>
  m [Database.EntityFilterParam]
getFilters = forM (Database.getEntityFilters @e @a) $ \case
  Database.EFString p -> Database.EFPTextOptional <$> getParam p
  Database.EFNum p -> Database.EFPIntOptional <$> getNumParam p
  Database.EFNumList p -> Database.EFPIntListOptional <$> getNumListParam p
  Database.EFDate p -> Database.EFPDateOptional <$> getDateParam p
  Database.EFLimit -> getLimit
  Database.EFOffset -> getOffset

getLimit :: Application m => m Database.EntityFilterParam
getLimit = do
  maxLimit <- asks (Database.cPagSize . cDB . envConfig)
  Database.EFPInt . maybe maxLimit (min maxLimit) <$> getNumParam "limit"

getOffset :: Application m => m Database.EntityFilterParam
getOffset = Database.EFPInt . fromMaybe 0 <$> getNumParam "offset"

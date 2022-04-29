{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Api.Get where

import Data.Aeson (ToJSON)
import Data.Coerce
import Data.Maybe

import Data.Data 
import Data.Text (Text)
import Data.Text qualified as T
import Data.Kind (Type)
import Data.List

import Database.Database (Database)
import Database.Database qualified as Database

import Logger qualified

import Entity.Internal qualified as Entity
import Entity.Internal (Entity(..))

import App.Result
import App.ResultJSON
import App.Types
import App.Router
import App.Internal
import App.Config


import HKD.HKD
import Control.Monad (forM)
import Control.Monad.Reader (asks)

type Gettable m e a =
    ( Database.Gettable e a
    , Database.ToRowOf (Database m) [ID (e a)]
    , Database.FromRowOf (Database m) (e a)
    , Database.ToRowOf (Database.Database m) [Database.EntityFilterParam]
    , Data (e a)
    , Show (e a)
    , Typeable e
    , Eq (e a)
    )

get_ :: forall (e :: Type -> Type) (m :: Type -> Type). 
    ( Application m
    , Gettable m (Entity e) (Front Display)
    , ToJSONResult (Entity e (Front Display))
    ) => Text -> Router e m ()
get_ p 
    | T.isSuffixOf "{ID}" p = get p (getEntity   @(Entity e) @(Front Display))
    | otherwise             = get p (getEntities @(Entity e) @(Front Display))


getEntity :: forall (e :: Type -> Type) a m. 
    ( Application m
    , Gettable m e a
    , ToJSONResult (e a)
    ) => Endpoint m
getEntity eIDs = do
    Logger.info $ "Attempt to get " <> nameOf @e
    entity <- Database.getEntityGeneric @e @m (map coerce eIDs)
    Logger.info $ nameOf @e <> " was found."
    json @_ @(e a) entity

getEntities :: forall (e :: Type -> Type) a m. 
    ( Application m
    , Gettable m e a
    , ToJSONResult (e a)
    ) => Endpoint m
getEntities _ = do
    Logger.info $ "Attempt to get " <> nameOf @e <> "s"
    fs <- getFilters @e @a
    entities <- Database.getEntitiesGeneric @e @a @m fs
    Logger.info $ nameOf @e <> " was found."
    json entities

getFilters :: forall e a m. 
    ( Application m
    , Gettable m e a
    ) => m [Database.EntityFilterParam]
getFilters = forM (sort $ Database.getEntityFilters @e @a) $ \case
    Database.EFString  p -> Database.EFPTextOptional <$> getParam p
    Database.EFNum     p -> Database.EFPIntOptional  <$> getNumParam p
    Database.EFNumList p -> Database.EFPIntOptional  <$> getNumParam p
    Database.EFDate    p -> Database.EFPDateOptional <$> getDateParam p
    Database.EFLimit     -> getLimit
    Database.EFOffset    -> getOffset

getLimit :: Application m => m Database.EntityFilterParam
getLimit = do
    maxLimit <- asks (Database.cPagSize . cDatabase . envConfig)
    Database.EFPInt . maybe maxLimit (min maxLimit) <$> getNumParam "limit"

getOffset :: Application m =>  m Database.EntityFilterParam
getOffset = Database.EFPInt . fromMaybe 0 <$> getNumParam "offset"




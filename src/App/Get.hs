{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuantifiedConstraints #-}

module App.Get where

import Control.Monad.Catch
import Data.Aeson (ToJSON)

import Data.Data
import Data.Text (Text)
import Data.Text qualified as T
import Data.Kind (Type)

import Database.Database qualified as Database

import Logger qualified

import Entity.Internal qualified as Entity

import App.Result
import App.Router
import App.Internal
import App.Types

import HKD.HKD

singleE :: Text -> Bool
singleE = T.isSuffixOf "{ID}"  

get_ :: forall (e :: Type -> Type) (m :: Type -> Type) env. 
    -- ( GettableManyFrom (Database m) e
    ( Monad m
    , MonadThrow m
    , HasEnv m
    , ToJSON (e (Front Display))
    , Database.GettableFrom (Database.Database m) e (Front Display)
    , Database.ToRowOf (Database.Database m) IDs
    , Database.ToRowOf (Database.Database m) [Page]
    , Database.FromRowOf (Database.Database m) (e (Front Display))
    , Database.HasDatabase m
    , Database.QConstraints (Database.Database m)
    , Typeable e
    , Data (e (Front Display))
    , Logger.HasLogger m
    ) =>
    Text -> Router e m ()
get_    p 
    | singleE p = get p (getEntity @e @(Front Display))
    | otherwise = get p (getEntities @e @(Front Display))

getEntities :: forall (e :: Type -> Type) a m env. 
    ( Monad m
    , MonadThrow m
    , HasEnv m
    , ToJSON (e a)
    , Database.GettableFrom (Database.Database m) e a
    , Database.ToRowOf (Database.Database m) [Page]
    , Database.FromRowOf (Database.Database m) (e a)
    , Database.HasDatabase m
    , Typeable e
    , Data (e a)
    , Logger.HasLogger m
    ) => IDs -> m AppResult
getEntities _ = do
    Logger.info $ "Attempt to get " <> Entity.nameOf @e <> "s"
    page <- getPage
    entities <- Database.getEntities @e @a @m page
    Logger.info $ Entity.nameOf @e <> " was found."
    json entities

getEntity :: forall (e :: Type -> Type) a m env. 
    ( Monad m
    , MonadThrow m
    , HasEnv m 
    , ToJSON (e a)
    , Database.GettableFrom (Database.Database m) e a
    , Database.ToRowOf (Database.Database m) IDs
    , Database.FromRowOf (Database.Database m) (e a)
    , Database.HasDatabase m
    , Typeable e
    , Data (e a)
    , Logger.HasLogger m
    ) => IDs -> m AppResult
getEntity ids = do
    Logger.info $ "Attempt to get " <> Entity.nameOf @e
    entity <- Database.getEntity @e @a @m ids
    Logger.info $ Entity.nameOf @e <> " was found."
    json entity
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuantifiedConstraints #-}

module App.Get where

import Data.Aeson (ToJSON)

import Data.Text (Text)
import Data.Text qualified as T
import Data.Kind (Type)

import Database.Database qualified as Database

import Logger qualified

import Entity.Internal qualified as Entity

import App.Result
import App.Types
import App.Router
import App.Internal

import HKD.HKD

singleE :: Text -> Bool
singleE = T.isSuffixOf "{ID}"  

get_ :: forall (e :: Type -> Type) (m :: Type -> Type). 
    ( Application m
    , Gettable m e (Front Display)
    , ToJSON (e (Front Display))
    ) =>
    Text -> Router e m ()
get_ p 
    | singleE p = get p (getEntity   @e @(Front Display))
    | otherwise = get p (getEntities @e @(Front Display))

getEntities :: forall (e :: Type -> Type) a m env. 
    ( Application m
    , Gettable m e a
    , ToJSON (e a)
    ) => Endpoint m
getEntities _ = do
    Logger.info $ "Attempt to get " <> nameOf @e <> "s"
    page <- getPage
    entities <- Database.getEntities @e @a @m page
    Logger.info $ nameOf @e <> " was found."
    json entities

getEntity :: forall (e :: Type -> Type) a m. 
    ( Application m
    , Gettable m e a
    , ToJSON (e a)
    ) => Endpoint m
getEntity ids = do
    Logger.info $ "Attempt to get " <> nameOf @e
    entity <- Database.getEntity @e @m ids
    Logger.info $ nameOf @e <> " was found."
    json @_ @(e a) entity
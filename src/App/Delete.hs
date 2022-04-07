{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuantifiedConstraints #-}

module App.Delete where

import Control.Monad.Catch
import Data.Aeson (ToJSON)

import Data.Data
import Data.Text (Text)
import Data.Text qualified as T
import Data.Kind (Type)
import Data.String
import Data.Coerce

import Database.Database qualified as Database

import Logger qualified

import Entity.Internal qualified as Entity

import App.Result
import App.Router
import App.Internal
import App.Types

import HKD.HKD

delete_ ::  forall (e :: Type -> Type) (m :: Type -> Type). 
    ( Application m
    , Deletable m e 
    ) =>
    Text -> Router e m ()
delete_ p = delete p (deleteEntity @e)

deleteEntity :: forall (e :: Type -> Type) (m :: Type -> Type). 
    ( Application m
    , Deletable m e 
    ) => Endpoint m
deleteEntity eID = do
    Logger.info $ "Attempt to delete " <> Entity.nameOf @e 
    Database.deleteEntity @e eID
    Logger.info $ Entity.nameOf @e <> " successfuly deleted."
    text "Successfuly deleted."

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuantifiedConstraints #-}

module App.Put where

import Control.Monad.Catch
import Data.Aeson (FromJSON)

import Data.Data
import Data.Text (Text)
import Data.Text qualified as T
import Data.Kind (Type)
import Data.String

import Database.Database qualified as Database

import Logger qualified

import Entity.Internal qualified as Entity

import App.Result
import App.Router
import App.Internal
import App.Types

import HKD.HKD

put_ ::  forall (e :: Type -> Type) (m :: Type -> Type). 
    ( Application m
    , Puttable m e (Front Update)
    , FromJSON (e (Front Update))
    ) =>
    Text -> Router e m ()
put_ p = put p (putEntity @e)

putEntity :: forall (e :: Type -> Type) m. 
    ( Application m
    , Puttable m e (Front Update)
    , FromJSON (e (Front Update))
    ) => Endpoint m
putEntity eID = do
    Logger.info $ "Attempt to update " <> Entity.nameOf @e
    e <- decodedBody @(e (Front Update))
    Database.putEntity @e @m eID e
    Logger.info $ Entity.nameOf @e <> " was found."
    text "Successfuly updated."

-- type instance ToOneRow (e (FrontUpdate)) IDs = ()
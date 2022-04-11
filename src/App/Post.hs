-- {-# LANGUAGE NoMonomorphismRestriction #-}
-- {-# LANGUAGE QuantifiedConstraints #-}

module App.Post where

import Data.Aeson (FromJSON)

import Data.Coerce (Coercible(..), coerce)
import Data.Text (Text)
import Data.Kind (Type)

import Database.Database qualified as Database

import Logger qualified

import Entity.Internal qualified as Entity

import App.Result
import App.Router
import App.Internal

import HKD.HKD

post_ ::  forall (e :: Type -> Type) (m :: Type -> Type). 
    ( Application m
    , Postable m e
    , FromJSON (e Create)
    ) => Text -> Router e m ()
post_ p = post p (postEntity @e)

postEntity :: forall (e :: Type -> Type) m. 
    ( Application m
    , Postable m e
    , FromJSON (e Create)
    ) => Endpoint m
postEntity _ = do
    Logger.info $ "Attempt to post " <> Entity.nameOf @e
    e <- decodedBody @(e Create)
    Database.postEntity @e @m (coerce e) >>= text


{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Api.Put where

import Data.Aeson (FromJSON)

import Data.Text (Text)
import Data.Kind (Type)
import Data.Data

import Database.Database (Database)
import Database.Database qualified as Database

import Logger qualified
import Logger ((.<))

import Entity.Internal qualified as Entity
import Entity.Internal(Entity(..))

import App.Result
import App.Router
import App.Types
import App.Internal

import HKD.HKD
import Data.Coerce

type Puttable m e a =
    ( Database.Puttable e a
    , Database.ToRowOf (Database m) (e a)
    , Database.ToRowOf (Database m) (ID (e a))
    , Data (e a)
    , Typeable e
    )

put_ ::  forall (e :: Type -> Type) (m :: Type -> Type). 
    ( Application m
    , Puttable m (Entity e) (Front Update)
    , FromJSON (e (Front Update))
    , Typeable e
    ) =>
    Text -> Router e m ()
put_ p = put p (putEntity @e)

putEntity :: forall (e :: Type -> Type) m. 
    ( Application m
    , Puttable m (Entity e) (Front Update)
    , FromJSON (e (Front Update))
    , Typeable e
    ) => Endpoint m
putEntity [eID] = do
    Logger.info $ "Attempt to update " <> nameOf @(Entity e)
    e <- decodedBody @(e (Front Update))
    Database.putEntity @e @m @(Front Update) (Entity (coerce eID) e)
    Logger.info $ nameOf @(Entity e) <> " was found."
    text @_ @String "Successfuly updated."
putEntity _ = entityIDArityMissmatch $ "putEntity " .< nameOf @(Entity e)
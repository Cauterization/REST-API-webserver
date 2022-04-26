{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Api.Delete where


import Data.Data
import Data.Coerce
import Data.Text (Text)
import Data.Kind (Type)

import Database.Database (Database)
import Database.Database qualified as Database

import Logger qualified

import App.Result
import App.Router
import App.Types

import HKD.HKD

type Deletable m e =
    ( Database.Deletable e Delete
    , Database.ToRowOf (Database m) [ID (e Delete)]
    , Data (e Delete)
    , Typeable e
    )

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
deleteEntity eIDs = do
    Logger.info $ "Attempt to delete " <> nameOf @e 
    Database.deleteEntity @e (map coerce eIDs)
    Logger.info $ nameOf @e <> " successfuly deleted."
    text @_ @Text "Successfuly deleted."

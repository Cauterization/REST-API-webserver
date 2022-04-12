{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuantifiedConstraints #-}

module App.Delete where


import Data.Text (Text)
import Data.Kind (Type)

import Database.Database qualified as Database

import Logger qualified

import App.Result
import App.Router
import App.Types

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
    Logger.info $ "Attempt to delete " <> nameOf @e 
    Database.deleteEntity @e eID
    Logger.info $ nameOf @e <> " successfuly deleted."
    text @_ @String "Successfuly deleted."

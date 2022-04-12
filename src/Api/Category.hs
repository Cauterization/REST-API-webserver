module Api.Category where

import App.Router
import App.Internal
import App.Types

import Control.Monad
import Control.Monad.Catch

import Data.Coerce

import Database.Database qualified as Database
import Database.Database (Database)

import Entity.Category

import HKD.HKD
import App.Result

import Logger qualified
import Extended.Text qualified as T

putCategory :: forall m.
    ( Application m
    , Database.GettableFrom (Database m) ID (Category (Front Update))
    , Database.ToRowOf (Database m) (ID (Category (Front Update)))
    , Database.PuttableTo (Database m) Category
    , Database.ToRowOf (Database m) CatPutFiels
    ) => Endpoint m
putCategory [cID] = do
    c <- decodedBody @(Category (Front Update))
    mapM_ (validate c) $ parent c
    Database.putEntity @_ @m [cID] c
    text @_ @String "Successfuly updated."
  where
    validate c parent = do
        parents <- Database.getEntitiesWith (parent) id
        Logger.error $ T.show parents
        when (cast cID `elem` parents) $ throwM CategoryCycle
    cast = coerce @_ @(ID (Category (Front Update)))
putCategory _ = entityIDArityMissmatch "put category api"


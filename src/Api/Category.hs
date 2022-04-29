module Api.Category where

import Api.Put

import App.Router
import App.Internal
import App.Types

import Control.Monad
import Control.Monad.Catch

import Data.Coerce


import Database.Database qualified as Database
import Database.Database (Database)

import Entity.Category
import Entity.Internal (Entity(..))

import HKD.HKD
import App.Result

import Logger qualified
import Extended.Text (Text)
import Extended.Text qualified as T

putCategory :: forall m.
    ( Application m
    , Puttable m (Entity Category) (Front Update)
    , Database.Gettable ID (Category (Front Update))
    , Database.FromRowOf (Database m) (ID (Category (Front Update)))
    , Database.ToRowOf (Database m) (ID (Category (Front Update)))
    ) => Endpoint m
putCategory [cID] = do
    c <- decodedBody @(Category (Front Update))
    mapM_ validate $ parent c
    Database.putEntity @_ @m $ Entity (coerce cID) c
    text @_ @Text "Successfuly updated."
  where
    validate parent = do
        parents <- Database.getEntitiesWith parent id
        when (cast cID `elem` parents) $ throwM CategoryCycle
    cast = coerce @_ @(ID (Category (Front Update)))
putCategory _ = entityIDArityMissmatch "put category api"



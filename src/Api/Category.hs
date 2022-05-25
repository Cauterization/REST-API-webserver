{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module Api.Category where

import Api.Put (Puttable)
import App.AppT (Application)
import App.Error (categoryCycleError, idArityMissmatchError)
import App.Getters (decodedBody)
import App.Result (Endpoint, text)
import App.Types (ID)
import Control.Monad (when)
import Data.Coerce (coerce)
import Database.Get qualified as Database
import Database.HasDatabase qualified as Database
import Database.Put qualified as Database
import Entity.Category (Category (parent))
import Entity.Internal (Entity (..))
import Extended.Text (Text)
import HKD.HKD (Front, Update)

putCategory ::
  forall m.
  ( Application m,
    Puttable m Category (Front Update),
    Database.FromRowOf m (ID (Category (Front Update)))
  ) =>
  Endpoint m
putCategory [cID] = do
  c <- decodedBody @(Category (Front Update))
  mapM_ validate $ parent c
  Database.putEntity $ Entity (coerce cID) c
  text @_ @Text "Successfuly updated."
  where
    validate parent = do
      parents <- Database.getEntitiesWith parent id
      when (coerce @_ @(ID (Category (Front Update))) cID `elem` parents) categoryCycleError
putCategory _ = idArityMissmatchError "put category api"

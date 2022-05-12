module Api.Category where

import Api.Put (Puttable)
import App.Result (Endpoint, text)
import App.Types (ID)
import App.Internal
    ( decodedBody, Application, categoryCycleError, idArityMissmatchError )
import Control.Monad (when)
import Data.Coerce (coerce)
import Database.Database qualified as Database
import Database.HasDatabase (Database)
import Entity.Category (Category (parent))
import Entity.Internal (Entity (..))
import Extended.Text (Text)
import HKD.HKD (Front, Update)

putCategory ::
  forall m.
  ( Application m,
    Puttable m (Entity Category) (Front Update),
    Database.Gettable ID (Category (Front Update)),
    Database.FromRowOf (Database m) (ID (Category (Front Update))),
    Database.ToRowOf (Database m) (ID (Category (Front Update)))
  ) =>
  Endpoint m
putCategory [cID] = do
  c <- decodedBody @(Category (Front Update))
  mapM_ validate $ parent c
  Database.putEntity @_ @m $ Entity (coerce cID) c
  text @_ @Text "Successfuly updated."
  where
    validate parent = do
      parents <- Database.getEntitiesWith parent id
      when (cast cID `elem` parents) $ categoryCycleError
    cast = coerce @_ @(ID (Category (Front Update)))
putCategory _ = idArityMissmatchError "put category api"

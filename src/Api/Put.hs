module Api.Put where

import App.Internal
  ( Application,
    decodedBody,
    idArityMissmatchError,
  )
import App.Result (Endpoint, text)
import App.Router (Router, put)
import App.Types (ID (ID), nameOf)
import Data.Aeson (FromJSON)
import Data.Coerce (coerce)
import Data.Data
import Data.Kind (Type)
import Data.Text (Text)
import Database.Database (Database)
import Database.Database qualified as Database
import Entity.Internal (Entity (..))
import HKD.HKD (Front, Update)
import Logger qualified

type Puttable m e a =
  ( Database.Puttable (e a),
    Database.ToRowOf (Database m) (e a),
    Database.ToRowOf (Database m) (ID (e a)),
    Data (e a),
    Typeable e
  )

put_ ::
  forall (e :: Type -> Type) (m :: Type -> Type).
  ( Application m,
    Puttable m (Entity e) (Front Update),
    FromJSON (e (Front Update)),
    Typeable e
  ) =>
  Text ->
  Router e m ()
put_ p = put p (putEntity @e)

putEntity ::
  forall (e :: Type -> Type) m.
  ( Application m,
    Puttable m (Entity e) (Front Update),
    FromJSON (e (Front Update)),
    Typeable e
  ) =>
  Endpoint m
putEntity [eID] = do
  Logger.info $ "Attempt to update " <> nameOf @(Entity e)
  e <- decodedBody @(e (Front Update))
  Database.putEntity @e @m @(Front Update) (Entity (coerce eID) e)
  Logger.info $ nameOf @(Entity e) <> " was found."
  text @_ @String "Successfuly updated."
putEntity _ = idArityMissmatchError $ "putEntity " <> nameOf @(Entity e)

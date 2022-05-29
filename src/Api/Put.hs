{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Api.Put where

import App.AppT (Application)
import App.Error (idArityMissmatchError)
import App.Getters (decodedBody)
import App.Result (Endpoint, text)
import App.Router (Router, put)
import App.Types (ID (ID), nameOf)
import Data.Aeson (FromJSON)
import Data.Coerce (coerce)
import Data.Data (Data, Typeable)
import Data.Kind (Type)
import Data.Text (Text)
import Database.HasDatabase qualified as Database
import Database.Put qualified as Database
import Entity.Internal (Entity (..))
import HKD.HKD (Front, Update)
import Logger qualified

type Puttable m e a =
  ( Database.Puttable (e a),
    Database.ToRowOf m (Entity e a),
    Database.ToRowOf m (ID (e a)),
    Data (e a),
    Typeable e
  )

put_ ::
  forall (e :: Type -> Type) (m :: Type -> Type).
  ( Application m,
    Puttable m e (Front Update),
    FromJSON (e (Front Update)),
    Typeable e
  ) =>
  Text ->
  Router e m ()
put_ p = put p (putEntity @e)

putEntity ::
  forall (e :: Type -> Type) m.
  ( Application m,
    Puttable m e (Front Update),
    FromJSON (e (Front Update)),
    Typeable e
  ) =>
  Endpoint m
putEntity [eID] = do
  Logger.info $ "Attempt to update " <> nameOf @(Entity e)
  e <- decodedBody @(e (Front Update))
  Database.putEntity @e @m @(Front Update) (Entity (coerce eID) e)
  Logger.info $ nameOf @(Entity e) <> " was found."
  text "Successfuly updated."
putEntity _ = idArityMissmatchError $ "putEntity " <> nameOf @(Entity e)

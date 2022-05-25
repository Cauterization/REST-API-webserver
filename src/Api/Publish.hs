{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Api.Publish where

import Api.Put (Puttable)
import App.AppT (Application)
import App.Error (idArityMissmatchError)
import App.Result (Endpoint, text)
import App.Router (Router, publish)
import App.Types (ID (ID), nameOf)
import Data.Coerce (coerce)
import Data.Data (Typeable)
import Data.Kind (Type)
import Data.Text (Text)
import Database.Put qualified as Database
import Entity.Internal (Entity (Entity))
import HKD.HKD (EmptyData (..), Publish)
import Logger qualified

publish_ ::
  forall (e :: Type -> Type) (m :: Type -> Type).
  ( Application m,
    Puttable m e Publish,
    Typeable e,
    EmptyData (e Publish)
  ) =>
  Text ->
  Router e m ()
publish_ p = publish p (publishEntity @e)

publishEntity ::
  forall (e :: Type -> Type) m.
  ( Application m,
    Puttable m e Publish,
    Typeable e,
    EmptyData (e Publish)
  ) =>
  Endpoint m
publishEntity [eID] = do
  Logger.info $ "Attempt to publish " <> nameOf @e
  Database.putEntity @e @m @Publish (Entity (coerce eID) emptyData)
  text @_ @String "Successfuly published."
publishEntity _ = idArityMissmatchError $ "putEntity " <> nameOf @e

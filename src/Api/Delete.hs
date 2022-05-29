{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module Api.Delete where

import App.AppT (Application)
import App.Result (Endpoint, text)
import App.Router (Router, delete)
import App.Types (ID (ID), nameOf)
import Data.Coerce (coerce)
import Data.Data (Data, Typeable)
import Data.Kind (Type)
import Data.Text (Text)
import Database.Delete qualified as Database
import Database.HasDatabase qualified as Database
import HKD.HKD (Delete)
import Logger qualified

type Deletable (m :: Type -> Type) (e :: Type -> Type) =
  ( Database.Deletable e,
    Database.ToRowOf m [ID (e Delete)],
    Data (e Delete),
    Typeable e
  )

delete_ ::
  forall (e :: Type -> Type) (m :: Type -> Type).
  ( Application m,
    Deletable m e
  ) =>
  Text ->
  Router e m ()
delete_ p = delete p (deleteEntity @e)

deleteEntity ::
  forall (e :: Type -> Type) (m :: Type -> Type).
  ( Application m,
    Deletable m e
  ) =>
  Endpoint m
deleteEntity eIDs = do
  Logger.info $ "Attempt to delete " <> nameOf @e
  Database.deleteEntity @e (map coerce eIDs)
  Logger.info $ nameOf @e <> " successfuly deleted."
  text "Successfuly deleted."

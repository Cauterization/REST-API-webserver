module Api.Publish where

import Data.Aeson (FromJSON)

import Data.Coerce (Coercible, coerce)
import Data.Text (Text)
import Data.Kind (Type)
import Data.Data

import Database.Database (Database)
import Database.Database qualified as Database

import Logger qualified

import App.Result
import App.Router
import App.Internal
import App.Types

import Api.Put

import Entity.Internal

import HKD.HKD

publish_ ::  forall (e :: Type -> Type) (m :: Type -> Type). 
    ( Application m
    , Puttable m (Entity e) Publish
    , Typeable e
    , EmptyData (e Publish)
    ) => Text -> Router e m ()
publish_ p = publish p (publishEntity @e)

publishEntity :: forall (e :: Type -> Type) m. 
    ( Application m
    , Puttable m (Entity e) Publish
    , Typeable e
    , EmptyData (e Publish)
    ) => Endpoint m
publishEntity [eID] = do
    Logger.info $ "Attempt to publish " <> nameOf @e
    Database.putEntity @e @m @Publish (Entity (coerce eID) emptyData)
    text @_ @String "Successfuly published."
publishEntity _ = entityIDArityMissmatch $ "putEntity " <> nameOf @e
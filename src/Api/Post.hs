module Api.Post where

import App.Internal (Application, decodedBody)
import App.Result (Endpoint, text)
import App.Router (Router, post)
import App.Types (ID (ID), nameOf)
import Data.Aeson (FromJSON)
import Data.Coerce (coerce)
import Data.Data (Data, Typeable)
import Data.Kind (Type)
import Data.Text (Text)
import Database.Database (Database)
import Database.Database qualified as Database
import Entity.Author (Author (..))
import Entity.Category (Category (..), CategoryName (CategoryName))
import Entity.Tag (Tag (..))
import HKD.HKD (Create, Front)
import Logger qualified

type Postable m e =
  ( Database.Postable e Create,
    Database.ToRowOf (Database m) (e Create),
    Database.FromRowOf (Database m) (ID (e Create)),
    Show (ID (e Create)),
    Data (e Create),
    Show (e Create),
    Typeable e
  )

post_ ::
  forall (e :: Type -> Type) (m :: Type -> Type).
  ( Application m,
    Postable m e,
    FromJSON (e (Front Create)),
    CreateFromFront e
  ) =>
  Text ->
  Router e m ()
post_ p = post p (postEntity @e)

postEntity ::
  forall (e :: Type -> Type) m.
  ( Application m,
    Postable m e,
    FromJSON (e (Front Create)),
    CreateFromFront e
  ) =>
  Endpoint m
postEntity _ = do
  Logger.info $ "Attempt to post " <> nameOf @e
  e <- fromFront =<< decodedBody @(e (Front Create))
  Database.postEntity @e @m e >>= text

class CreateFromFront e where
  fromFront :: Application m => e (Front Create) -> m (e Create)

instance CreateFromFront Tag where
  fromFront Tag {..} = pure $ Tag {..}

instance CreateFromFront Category where
  fromFront Category {..} = pure $ Category {parent = coerce parent, name = coerce name}

instance CreateFromFront Author where
  fromFront Author {..} = pure $ Author {user = coerce user, ..}

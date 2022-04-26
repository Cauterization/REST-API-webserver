-- {-# LANGUAGE NoMonomorphismRestriction #-}
-- {-# LANGUAGE QuantifiedConstraints #-}

module Api.Post where

import Data.Aeson (FromJSON)

import Data.Coerce (Coercible(..), coerce)
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

import Entity.Author
import Entity.Tag
import Entity.Category

import HKD.HKD

type Postable m e =
    ( Database.Postable e Create
    , Database.ToRowOf (Database m) (e Create)
    , Database.FromRowOf (Database m) (ID (e Create))
    , Show (ID (e Create))
    , Data (e Create)
    , Show (e Create)
    , Typeable e
    )

post_ ::  forall (e :: Type -> Type) (m :: Type -> Type). 
    ( Application m
    , Postable m e
    , FromJSON (e (Front Create))
    , CreateFromFront e
    ) => Text -> Router e m ()
post_ p = post p (postEntity @e)

postEntity :: forall (e :: Type -> Type) m. 
    ( Application m
    , Postable m e
    , FromJSON (e (Front Create))
    , CreateFromFront e
    ) => Endpoint m
postEntity _ = do
    Logger.info $ "Attempt to post " <> nameOf @e
    e <- fromFront =<< decodedBody @(e (Front Create))
    Database.postEntity @e @m e >>= text

class CreateFromFront e where
    fromFront :: Application m => e (Front Create) -> m (e Create)

instance CreateFromFront Tag where
    fromFront Tag{..} = pure $ Tag{..}

instance CreateFromFront Category where
    fromFront Category{..} = pure $ Category{parent = coerce parent, name = coerce name}

instance CreateFromFront Author where
    fromFront Author{..} = pure $ Author{user = coerce user, ..}

-- postAuthor :: forall m.
--     ( Application m
--     , Postable m Author
--     ) => Endpoint m
-- postAuthor _ = do
--     Author{..} <- decodedBody @(Author (Front Create))
--     Logger.info "Attempt to post author"
--     Database.postEntity () >>= text
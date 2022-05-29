module Mocks.Get where

import Data.Coerce
import Entity.Internal
import Entity.Tag
import HKD.HKD

class ToFrontDisplay e where
  toFrontDisplay :: e Display -> e (Front Display)

instance ToFrontDisplay Tag where
  toFrontDisplay Tag {..} = Tag {..}

getTransform :: ToFrontDisplay e => Entity e Display -> Entity e (Front Display)
getTransform (Entity eID e) = Entity (coerce eID) (toFrontDisplay e)

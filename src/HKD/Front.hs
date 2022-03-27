module HKD.Front where


import HKD.Field ( Field )
import HKD.Utils ( If, Contains )
import Data.Aeson 

import qualified Extended.Postgres as Postgres

import GHC.Generics

data Front a
data NotAllowedFromFront deriving Generic

instance FromJSON NotAllowedFromFront where
  parseJSON _ = fail "Can't specify this field"

instance Postgres.ToField NotAllowedFromFront where
  toField _ = Postgres.renderNull

type instance Field name req (Front b) modifiers a =
  If (Contains NotAllowedFromFront modifiers) 
     (Maybe NotAllowedFromFront)
     (Field name req b modifiers a)


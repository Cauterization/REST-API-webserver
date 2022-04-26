{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDeriving #-}
module HKD.Front where


import HKD.Create (Create)
import HKD.Display (Display, Hidden, NotDisplayed)
import HKD.Update (Update)

import HKD.Field ( Field )
import HKD.Utils ( If, Contains )
import Data.Aeson 

import qualified Extended.Postgres as Postgres

import GHC.Generics
import Data.Data

data Front a
deriving instance Data a => Data (Front a)
data NotAllowedFromFront deriving (Generic, Data, Show)

instance FromJSON NotAllowedFromFront where
  parseJSON _ = fail "Can't specify this field"

instance Postgres.ToField NotAllowedFromFront where
  toField _ = Postgres.renderNull

type instance Field req (Front Create) modifiers a =
  If (Contains NotAllowedFromFront modifiers) 
     (Maybe NotAllowedFromFront)
     (Field req Create modifiers a)

type instance Field req (Front Update) modifiers a =
  If (Contains NotAllowedFromFront modifiers) 
     (Maybe NotAllowedFromFront)
     (Field req Update modifiers a)

type instance Field req (Front Display) modifiers a =
  If (Contains Hidden modifiers) 
     (Maybe NotDisplayed)
     (Field req Display modifiers a)
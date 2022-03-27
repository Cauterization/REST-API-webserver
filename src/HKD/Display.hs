{-# LANGUAGE DeriveDataTypeable #-}
module HKD.Display where

import HKD.EmptyData
import HKD.Field
import HKD.Utils
import Data.Aeson 

import qualified Extended.Postgres as Postgres

import GHC.Generics
import Data.Data

data Display :: *
    deriving Data

data Hidden
data NotDisplayed deriving (Generic, ToJSON, Data)

instance {-# OVERLAPPING #-} Postgres.FromField (Maybe NotDisplayed) where
    fromField = const . flip (Postgres.returnError Postgres.ConversionFailed) "Can't display this field"

type instance Field name req Display modifiers a = 
    If (Contains Hidden modifiers) 
        (Maybe (ApplyRequired req Maybe NotDisplayed)) 
        (ApplyRequired req Maybe a)

display :: EmptyData (e Display) => (e Display -> e Display) -> e Display
display = ($ emptyData) 


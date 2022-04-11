{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDeriving #-}
module HKD.Display where

import HKD.EmptyData
import HKD.Field
import Data.Aeson 

import qualified Extended.Postgres as Postgres

import GHC.Generics
import Data.Data

data Display deriving (Data)

-- instance (Show NotDisplayed)

data Hidden
data NotDisplayed deriving (Generic, ToJSON, Data, Show)

instance {-# OVERLAPPING #-} Postgres.FromField (Maybe NotDisplayed) where
    fromField _ _ = pure Nothing

type instance Field name req Display modifiers a = ApplyRequired req Maybe a

display :: EmptyData (e Display) => (e Display -> e Display) -> e Display
display = ($ emptyData) 


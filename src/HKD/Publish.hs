{-# LANGUAGE DeriveDataTypeable #-}
module HKD.Publish where

import Data.Data

import HKD.Field
import HKD.Utils
import HKD.Update

data Publish deriving Data

type instance Field Publish modifiers a = 
    Maybe (Maybe NotUpdated)
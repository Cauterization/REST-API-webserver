{-# LANGUAGE DeriveDataTypeable #-}
module HKD.Publish where

import Data.Data

import HKD.Field
import HKD.Utils

data Publish deriving Data

data NoPublish deriving Data

type instance Field name req Publish modifiers a = 
    If (Contains NoPublish modifiers)
    (Maybe NoPublish)
    (ApplyRequired req Maybe a)
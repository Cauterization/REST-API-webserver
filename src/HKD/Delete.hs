{-# LANGUAGE DeriveDataTypeable #-}
module HKD.Delete where

import Data.Data

import HKD.Field

data Delete deriving Data

type instance Field Delete modifiers a = Maybe a
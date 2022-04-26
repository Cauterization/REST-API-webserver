{-# LANGUAGE DeriveDataTypeable #-}
module HKD.Delete where

import Data.Data

import HKD.Field

data Delete deriving Data

type instance Field req Delete modifiers a = ApplyRequired req Maybe a
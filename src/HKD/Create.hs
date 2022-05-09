{-# LANGUAGE DeriveDataTypeable #-}
module HKD.Create where

import HKD.Field

import Data.Data

data Create deriving Data

type instance Field Create modifiers a = a
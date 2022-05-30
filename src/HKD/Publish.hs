module HKD.Publish where

import Data.Data (Data)
import HKD.Field (Field)
import HKD.Update (NotUpdated)

data Publish deriving (Data)

type instance
  Field Publish modifiers a =
    Maybe (Maybe NotUpdated)

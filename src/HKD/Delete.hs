module HKD.Delete where

import Data.Data (Data)
import HKD.Field (Field)

data Delete deriving (Data)

type instance Field Delete modifiers a = Maybe a

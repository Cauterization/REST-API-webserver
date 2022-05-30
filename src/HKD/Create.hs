module HKD.Create where

import Data.Data (Data)
import HKD.Field (Field)

data Create deriving (Data)

type instance Field Create modifiers a = a

module HKD.Create where

import HKD.Field

data Create

type instance Field name req Create modifiers a = ApplyRequired req Maybe a
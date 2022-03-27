module HKD.Delete where

import HKD.Field

data Delete

type instance Field name req Delete modifiers a = ApplyRequired req Maybe a
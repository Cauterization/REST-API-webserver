module HKD.Field where

import GHC.TypeLits

type family Field action (modifiers :: [*]) a :: *
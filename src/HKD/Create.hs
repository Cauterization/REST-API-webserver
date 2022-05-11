{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module HKD.Create where

import Data.Data
import HKD.Field

data Create deriving (Data)

type instance Field Create modifiers a = a

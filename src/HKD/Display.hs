{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module HKD.Display where

import Data.Aeson
import Data.Data
import Extended.Postgres qualified as Postgres
import GHC.Generics
import HKD.EmptyData
import HKD.Field

data Display deriving (Data)

-- instance (Show NotDisplayed)

data Hidden

data NotDisplayed deriving (Generic, ToJSON, Data, Show, Eq)

instance {-# OVERLAPPING #-} Postgres.FromField (Maybe NotDisplayed) where
  fromField _ _ = pure Nothing

type instance Field Display modifiers a = a

display :: EmptyData (e Display) => (e Display -> e Display) -> e Display
display = ($ emptyData)

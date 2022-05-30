{-# LANGUAGE ImportQualifiedPost #-}

module HKD.Display where

import Data.Aeson (ToJSON)
import Data.Data (Data)
import Extended.Postgres qualified as Postgres
import GHC.Generics (Generic)
import HKD.EmptyData (EmptyData (..))
import HKD.Field (Field)

data Display deriving (Data)

data Hidden

data NotDisplayed deriving (Generic, ToJSON, Data, Show, Eq)

instance {-# OVERLAPPING #-} Postgres.FromField (Maybe NotDisplayed) where
  fromField _ _ = pure Nothing

type instance Field Display modifiers a = a

display :: EmptyData (e Display) => (e Display -> e Display) -> e Display
display = ($ emptyData)

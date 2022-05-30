{-# LANGUAGE ImportQualifiedPost #-}

module HKD.Update where

import Data.Aeson qualified as J
import Data.Data (Data)
import Extended.Postgres qualified as Postgres
import HKD.EmptyData (EmptyData (..))
import HKD.Field (Field)
import HKD.Utils (Contains, If)

data Update deriving (Data)

data Immutable

data NotUpdated deriving (Data, Eq, Ord)

instance J.FromJSON NotUpdated where
  parseJSON _ = fail "Can't update this Field"

instance Postgres.ToField NotUpdated where
  toField _ = Postgres.renderNull

type instance
  Field Update modifiers a =
    If
      (Contains Immutable modifiers)
      (Maybe NotUpdated)
      (Maybe a)

update :: EmptyData (e Update) => (e Update -> e Update) -> e Update
update = ($ emptyData)

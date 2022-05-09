{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDeriving #-}
module HKD.Update where

import HKD.EmptyData
import HKD.Field
import HKD.Utils
import Data.Aeson qualified as J

import qualified Extended.Postgres as Postgres
import Data.Data

data Update deriving Data

data Immutable
data NotUpdated deriving (Data, Eq, Ord)

instance J.FromJSON NotUpdated where
  parseJSON _ = fail "Can't update this Field"

instance Postgres.ToField NotUpdated where
  toField _ = Postgres.renderNull

type instance Field Update modifiers a = 
  If (Contains Immutable modifiers) 
     (Maybe NotUpdated)
     (Maybe a)

update :: EmptyData (e Update) => (e Update -> e Update) -> e Update
update = ($ emptyData) 
module Database.EntityFilters where

import App.Types (Date)
import Extended.Postgres qualified as Postgres
import Extended.Text (Text)

data EntityFilter
  = EFString !Text
  | EFNum !Text
  | EFNumList !Text
  | EFDate !Text
  | EFLimit
  | EFOffset
  deriving (Eq)

instance Ord EntityFilter where
  compare EFOffset _ = GT
  compare _ EFOffset = LT
  compare EFLimit _ = GT
  compare _ EFLimit = LT
  compare _ _ = EQ

data EntityFilterParam
  = EFPInt !Int
  | EFPText !Text
  | EFPTextOptional !(Maybe Text)
  | EFPIntOptional !(Maybe Int)
  | EFPIntListOptional !(Maybe [Int])
  | EFPDateOptional !(Maybe Date)
  deriving (Show)

instance Postgres.ToField EntityFilterParam where
  toField = \case
    EFPInt i -> Postgres.toField i
    EFPText t -> Postgres.toField t
    EFPIntOptional i -> Postgres.toField i
    EFPIntListOptional il -> Postgres.toField $ Postgres.PGArray <$> il
    EFPTextOptional t -> Postgres.toField t
    EFPDateOptional d -> Postgres.toField d

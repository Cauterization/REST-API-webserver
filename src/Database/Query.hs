{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Database.Query where

import Data.String (IsString(..))

import Database.Internal

-- | A newtype for database queries
-- an associated type is needed here in order 
-- to be able to make query a class method

newtype Query db e = Query {unQuery :: QueryOf db}

deriving newtype instance IsString (QueryOf db) => IsString (Query db e)
deriving newtype instance Semigroup (QueryOf db) => Semigroup (Query db e)
deriving newtype instance Monoid (QueryOf db) => Monoid (Query db e)

type QConstraints db = (IsString (QueryOf db), Monoid (QueryOf db), Show (QueryOf db))


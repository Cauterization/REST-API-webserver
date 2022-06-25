{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mocks.Entity.Draft where

import App.Types (Token)
import Control.Monad.State (gets, join, modify)
import Data.Aeson (ToJSON)
import Database.EntityFilters qualified as Database
import Entity.Article (Article)
import Entity.Draft (Draft (Draft))
import Entity.Internal (Entity)
import HKD.HKD (Create, Display, Front, Publish, Update)
import Mocks.Entity.Article ()
import Mocks.TestMonad
  ( TestEntity (..),
    TestState (..),
  )
import Test.QuickCheck (Arbitrary (arbitrary))

deriving newtype instance ToJSON (Draft (Front Create))

deriving newtype instance ToJSON (Draft (Front Display))

deriving newtype instance ToJSON (Draft (Front Update))

instance Arbitrary (Article a) => Arbitrary (Draft a) where
  arbitrary = Draft <$> arbitrary

instance TestEntity (Draft Create)

instance TestEntity (Entity Draft (Front Update))

instance TestEntity (Entity Draft (Front Display)) where
  getFromState = join $ gets getDrafts
  withGetEntities drafts s = s {getDrafts = pure drafts}

instance TestEntity (Entity Draft Publish)

instance TestEntity (Token, Database.EntityFilterParam, Database.EntityFilterParam) where
  addToState (token, limit, offset) =
    modify (\TestState {..} -> TestState {filters = [limit, offset], userToken = Just token, ..})

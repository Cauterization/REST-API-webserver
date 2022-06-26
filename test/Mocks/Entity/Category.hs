module Mocks.Entity.Category where

import App.Types (ID)
import Control.Monad.State (gets, join)
import Data.Aeson (ToJSON)
import Entity.Category
  ( CatParent,
    Category (..),
    CategoryName (..),
  )
import Entity.Internal (Entity(..))
import Extended.Text (Text)
import HKD.HKD (Create, Display, Field, Front, Update)
import Mocks.Arbitrary ()
import Mocks.TestMonad
  ( TestEntity (getFromState, getFromTestDatabase, withGetEntities),
    TestState (getCategories, getCategoriesID),
  )
import Test.QuickCheck (Arbitrary (arbitrary))

deriving instance (ToJSON (Category (Front Create)))

deriving instance (ToJSON (Category (Front Update)))

instance TestEntity (Category a)

instance Arbitrary (CategoryName a) where
  arbitrary = CategoryName <$> arbitrary

instance
  ( Arbitrary (Field a '[] (CategoryName a)),
    Arbitrary (Field a '[] (CatParent a))
  ) =>
  Arbitrary (Category a)
  where
  arbitrary = Category <$> arbitrary <*> arbitrary

instance TestEntity (Entity Category (Front Update))

instance {-# OVERLAPPING #-} TestEntity (ID (Category (Front Update))) where
  getFromTestDatabase _ = join $ gets getCategoriesID
  withGetEntities catIDs s = s {getCategoriesID = pure catIDs}

instance TestEntity (Entity Category (Front Display)) where
  getFromState = join $ gets getCategories
  withGetEntities cats s = s {getCategories = pure cats}

getCatNames :: Entity Category (Front Display) -> [Text]
getCatNames (Entity _ (Category{..})) = let name' = unCatName name 
  in maybe [name'] ((name' :) . getCatNames) parent
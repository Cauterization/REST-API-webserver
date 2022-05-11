{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Helpers.Category where

import Data.Aeson ( ToJSON )
import Entity.Category
    ( CatParent, CategoryName(CategoryName), Category(Category) )
import Extended.Text qualified as T
import HKD.HKD ( Display, Front, Create, Field )
import Helpers.Internal ()
import Test.QuickCheck ( Arbitrary(arbitrary) )

deriving instance
  ( Ord (Field a '[] (CategoryName a)),
    Ord (Field a '[] (CatParent a))
  ) =>
  Ord (Category a)

instance Arbitrary (CategoryName a) where
  arbitrary = CategoryName <$> arbitrary

-- | Post
deriving instance ToJSON (Category Create)

instance Arbitrary (Category Create) where
  arbitrary = Category <$> arbitrary <*> arbitrary

deriving instance ToJSON (Category (Front Create))

instance Arbitrary (Category Display) where
  arbitrary = Category <$> arbitrary <*> arbitrary



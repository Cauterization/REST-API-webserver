{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Helpers.Category where

import App.Types

import Data.Aeson
import Data.Coerce
import Data.List.Split

import Entity.Category
import Entity.Internal
import Extended.Text qualified as T
import HKD.HKD
import Helpers.Internal

import Test.QuickCheck

deriving instance Ord  (CategoryName a)
instance Arbitrary     (CategoryName a) where
    arbitrary = CategoryName <$> arbitrary

-- | Post

-- deriving instance Show (Category Create)
deriving instance Eq     (Category Create)
deriving instance Ord    (Category Create)
deriving instance ToJSON (Category Create)

instance Arbitrary (Category Create) where
    arbitrary = Category <$> arbitrary <*> arbitrary

deriving instance ToJSON   (Category (Front Create))

deriving instance Show (Category Display)
deriving instance Eq   (Category Display)
deriving instance Ord  (Category Display)

instance Arbitrary (Category Display) where
    arbitrary = Category <$> arbitrary <*> arbitrary












-- deriving instance FromJSON (Category (Front Create))
-- deriving instance ToJSON   (Category (Front Create))

-- deriving newtype instance Ord (CategoryName a)
-- deriving instance Eq     (Category Create)
-- deriving instance Ord    (Category Create)
-- deriving instance ToJSON (Category Create)
-- instance Arbitrary       (Category Create) where
--     arbitrary = Category <$> arbitrary <*> arbitrary

-- deriving instance Eq       (Category (Front Display))
-- deriving instance Ord      (Category (Front Display))

-- instance FromJSON (Category (Front Display)) where
--     parseJSON = withObject "category front display" $ \o -> do
--         c@(_:_) <- map CategoryName . T.splitOn "/" <$> o .: "category"
--         pure Category{parent = reverse $ init c, name =  coerce $ last c}


-- instance ToJSON           (Category (Front Display)) where
--     toJSON Category{..} = object 
--         [ "category" .= T.intercalate "/" (reverse $ map unCatName parent)
--         ]
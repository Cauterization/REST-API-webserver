{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Helpers.Draft where


import App.Types

import Data.Aeson
import Data.Coerce
import Data.Text (Text)

import Entity.Article
import Entity.Draft
import Entity.Author
import Entity.Picture
import Entity.Category
import Entity.Internal
import Entity.Tag

import HKD.HKD
import Helpers.Article
import Helpers.Internal
import Helpers.User
import Helpers.Tag
import Helpers.Category
import Helpers.Author

import Test.QuickCheck

instance Arbitrary (Article a) => Arbitrary (Draft a) where
    arbitrary = Draft <$> arbitrary

deriving instance (Ord (Article a), Eq (Draft a)) => Ord (Draft a) 

deriving instance ToJSON (Article (Front Create))
deriving newtype instance ToJSON (Draft   (Front Create))

deriving instance ToJSON (Article (Front Update))
deriving newtype instance ToJSON (Draft (Front Update))
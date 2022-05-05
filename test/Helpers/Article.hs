module Helpers.Article where

import App.Types

import Data.Aeson
import Data.Coerce
import Data.Text (Text)

import Entity.Article
import Entity.Author
import Entity.Picture
import Entity.Category
import Entity.Internal
import Entity.Tag

import HKD.HKD
import Helpers.Internal
import Helpers.User
import Helpers.Tag
import Helpers.Category
import Helpers.Author

import Test.QuickCheck

instance Arbitrary (Article Display) where
    arbitrary = do
        title     <- arbitrary
        created   <- arbitrary
        content   <- arbitrary
        author    <- arbitrary
        category  <- arbitrary
        tags      <- arbitrary
        pics      <- arbitrary
        pure Article{..}

deriving instance 
    ( Ord (Field 'Required a '[]                               Text)
    , Ord (Field 'Required a '[NotAllowedFromFront, Immutable] Date)
    , Ord (Field 'Required a '[]                               Text)
    , Ord (Field 'Required a '[NotAllowedFromFront, Immutable] (EntityOrID Author a))
    , Ord (Field 'Required a '[]                               (EntityOrID Category a))
    , Ord (Field 'Required a '[]                               [EntityOrID Tag a])
    , Ord (Field 'Required a '[] [ID (Picture a)])
    ) => Ord (Article a)
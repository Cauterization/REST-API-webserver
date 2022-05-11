module Helpers.Article where

import App.Types
import Data.Aeson
import Data.Coerce
import Data.Text (Text)
import Entity.Article
import Entity.Author
import Entity.Category
import Entity.Internal
import Entity.Picture
import Entity.Tag
import HKD.HKD
import Helpers.Author
import Helpers.Category
import Helpers.Internal
import Helpers.Tag
import Helpers.User
import Test.QuickCheck

instance
  ( Arbitrary (Field a '[NotAllowedFromFront, Immutable] (EntityOrID Author a)),
    Arbitrary (Field a '[] (EntityOrID Category a)),
    Arbitrary (Field a '[] [EntityOrID Tag a]),
    Arbitrary (Field a '[] [ID (Picture a)]),
    Arbitrary (Field a '[NotAllowedFromFront, Immutable] Date),
    Arbitrary (Field a '[] Text)
  ) =>
  Arbitrary (Article a)
  where
  arbitrary = do
    title <- arbitrary
    created <- arbitrary
    content <- arbitrary
    author <- arbitrary
    category <- arbitrary
    tags <- arbitrary
    pics <- arbitrary
    pure Article {..}

deriving instance
  ( Ord (Field a '[] Text),
    Ord (Field a '[NotAllowedFromFront, Immutable] Date),
    Ord (Field a '[] Text),
    Ord (Field a '[NotAllowedFromFront, Immutable] (EntityOrID Author a)),
    Ord (Field a '[] (EntityOrID Category a)),
    Ord (Field a '[] [EntityOrID Tag a]),
    Ord (Field a '[] [ID (Picture a)])
  ) =>
  Ord (Article a)

module Helpers.Article where

import App.Types ( ID, Date )
import Data.Text (Text)
import Entity.Article ( Article(..) )
import Entity.Author ( Author )
import Entity.Category ( Category )
import Entity.Internal ( EntityOrID )
import Entity.Picture ( Picture )
import Entity.Tag ( Tag )
import HKD.HKD ( NotAllowedFromFront, Field, Immutable )
import Test.QuickCheck ( Arbitrary(arbitrary) )

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

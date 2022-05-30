module Mocks.Entity.Author where

import Control.Monad.State (gets, join)
import Data.Aeson (ToJSON)
import Entity.Author (Author (Author))
import Entity.Internal (Entity, EntityOrID)
import Entity.User (User)
import Extended.Text (Text)
import HKD.HKD (Create, Display, Field, Front, Immutable, Update)
import Mocks.Entity.User ()
import Mocks.TestMonad
  ( TestEntity (getFromState, withGetEntities),
    TestState (getAuthors),
  )
import Test.QuickCheck (Arbitrary (arbitrary))

deriving instance ToJSON (Author (Front Create))

deriving instance ToJSON (Author (Front Update))

instance
  ( Arbitrary (Field a '[Immutable] (EntityOrID User a)),
    Arbitrary (Field a '[] Text)
  ) =>
  Arbitrary (Author a)
  where
  arbitrary = Author <$> arbitrary <*> arbitrary

instance TestEntity (Author Create)

instance TestEntity (Entity Author (Front Update))

instance TestEntity (Entity Author (Front Display)) where
  getFromState = join $ gets getAuthors
  withGetEntities authors s = s {getAuthors = pure authors}

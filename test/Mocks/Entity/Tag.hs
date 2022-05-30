module Mocks.Entity.Tag where

import Control.Monad.State (gets, join)
import Data.Aeson (ToJSON)
import Entity.Internal (Entity)
import Entity.Tag (Tag (Tag))
import Extended.Text (Text)
import HKD.HKD (Create, Display, Field, Front, Update)
import Mocks.TestMonad
  ( TestEntity (getFromState, withGetEntities),
    TestState
      ( TestState,
        deleteResult,
        filters,
        getArticles,
        getAuthors,
        getCategories,
        getCategoriesID,
        getDrafts,
        getEntityPictures,
        getPictures,
        getTags,
        getUsers,
        getUsersDisplay,
        postResult,
        putResult,
        userToken
      ),
  )
import Test.QuickCheck (Arbitrary (arbitrary))

deriving instance ToJSON (Tag (Front Create))

deriving instance ToJSON (Tag (Front Update))

instance Arbitrary (Field a '[] Text) => Arbitrary (Tag a) where
  arbitrary = Tag <$> arbitrary

instance TestEntity (Tag a)

instance TestEntity (Entity Tag (Front Update))

instance TestEntity (Entity Tag (Front Display)) where
  getFromState = join $ gets getTags
  withGetEntities tags TestState {..} = TestState {getTags = pure tags, ..}

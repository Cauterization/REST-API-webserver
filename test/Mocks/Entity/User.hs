module Mocks.Entity.User where

import App.Types (Date, Token)
import Control.Monad.State (gets, join, modify)
import Data.Aeson
  ( Options (fieldLabelModifier, omitNothingFields),
    ToJSON (toJSON),
    camelTo2,
    defaultOptions,
    genericToJSON,
  )
import Entity.Internal (Entity)
import Entity.User (Auth, AuthField, User (..), UserFieldsConstraint)
import Extended.Text (Text)
import HKD.HKD
  ( Create,
    Display,
    Field,
    Front,
    Hidden,
    Immutable,
    NotAllowedFromFront,
  )
import Mocks.TestMonad
  ( TestEntity
      ( addToState,
        getFromState,
        getFromTestDatabase,
        withGetEntities
      ),
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
import Mocks.Utils ()
import Test.QuickCheck (Arbitrary (arbitrary))

instance ToJSON (User (Front Create)) where
  toJSON =
    genericToJSON
      defaultOptions
        { omitNothingFields = True,
          fieldLabelModifier = camelTo2 '_'
        }

deriving instance ToJSON (User Auth)

instance
  ( UserFieldsConstraint a Arbitrary
  ) =>
  Arbitrary (User a)
  where
  arbitrary = do
    firstName <- arbitrary
    lastName <- arbitrary
    login <- arbitrary
    token <- arbitrary
    password <- arbitrary
    registered <- arbitrary
    admin <- arbitrary
    pure User {..}

instance TestEntity (User Create)

instance TestEntity [Token] where
  addToState [t] = modify $ \TestState {..} -> TestState {userToken = Just t, ..}
  addToState _ = undefined

instance TestEntity (Entity User Display) where
  getFromTestDatabase _ = join $ gets getUsersDisplay
  withGetEntities users TestState {..} = TestState {getUsersDisplay = pure users, ..}

instance TestEntity (Entity User (Front Display)) where
  getFromState = join $ gets getUsers
  withGetEntities users TestState {..} = TestState {getUsers = pure users, ..}

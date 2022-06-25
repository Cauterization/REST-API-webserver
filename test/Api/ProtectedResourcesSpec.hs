module Api.ProtectedResourcesSpec where

import App.Result (AppResult (ResText))
import App.Types (Token)
import Entity.Internal (Entity (Entity))
import Entity.User (User (admin, token))
import HKD.HKD (Display)
import Mocks.Predicates
  ( isAdminAccessViolationError,
    isEntityNotFoundError,
    isUnathorizedError,
  )
import Mocks.Run (evalTest)
import Mocks.TestMonad (TestEntity (withGetEntities))
import Mocks.With (withPostPath, withToken, woLogger)
import Test.Hspec (Spec, context, it, shouldBe, shouldSatisfy)
import Test.QuickCheck (Property, Testable (property))

spec :: Spec
spec = do
  it "Allows to get protected resource when admin token is provided" $
    property propProtected
  context "Doesn't do it otherwise" $ do
    it "Non-admin" $ property propProtectedNonAdmin
    it "Wrong token" $ property propProtectedWrongToken
    it "No token" $ property propProtectedNoToken

propProtected :: Entity User Display -> Property
propProtected (Entity eID user) = property $ do
  res <-
    evalTest
      (withPostPath "admin/PRtest" . withToken (token user))
      (withGetEntities @(Entity User Display) [Entity eID user {admin = True}])
  res `shouldBe` Right (ResText "ok")

propProtectedNonAdmin :: Entity User Display -> Property
propProtectedNonAdmin (Entity eID user) = property $ do
  Left err <-
    evalTest
      ( withPostPath "admin/PRtest"
          . withToken (token user)
          . woLogger
      )
      (withGetEntities @(Entity User Display) [Entity eID user {admin = False}])
  err `shouldSatisfy` isAdminAccessViolationError

propProtectedWrongToken :: Token -> Property
propProtectedWrongToken t = property $ do
  Left err <-
    evalTest
      (withPostPath "admin/PRtest" . withToken t)
      id
  err `shouldSatisfy` isEntityNotFoundError

propProtectedNoToken :: Property
propProtectedNoToken = property $ do
  Left err <-
    evalTest
      (withPostPath "admin/PRtest")
      id
  err `shouldSatisfy` isUnathorizedError

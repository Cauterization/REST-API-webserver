module Api.CategorySpec where

import App.Result (AppResult (ResText))
import App.Types (ID (ID))
import Data.ByteString.Lazy qualified as BL
import Data.Coerce (coerce)
import Data.Maybe (isJust)
import Entity.Category (Category (..))
import Entity.Internal (Entity (Entity))
import Extended.Text qualified as T
import HKD.HKD (Front, Update)
import Mocks.Predicates
  ( isCategoryCycleError,
    isEntityNotFoundError,
  )
import Mocks.Run (evalTest)
import Mocks.TestMonad (TestEntity (withGetEntities))
import Mocks.With
  ( withBody,
    withFailedPut,
    withPutPath,
    woLogger,
  )
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.QuickCheck (Property, Testable (property), (==>))

spec :: Spec
spec = describe "PUT" $ do
  it "It actually updates category when all is ok" $
    property propPut
  it "Throws an appropriate error when there is no category with that ID" $
    property propPutDoesntExists
  it "Throws an appropriate error when put update forms a cycle in the category tree" $
    property propPutCategoryCycle

propPut ::
  Category (Front Update) ->
  ID (Category Update) ->
  Property
propPut c@Category {..} eID =
  property $
    parent /= Just (coerce eID) ==> do
      res <-
        evalTest
          (withPutPath ("categories/" <> T.show eID) . withBody c)
          id
      res `shouldBe` Right (ResText "Successfuly updated.")

propPutDoesntExists ::
  Category (Front Update) ->
  ID (Category Update) ->
  Property
propPutDoesntExists c@Category {..} eID =
  property $
    parent /= Just (coerce eID) ==> do
      Left err <-
        evalTest
          (withPutPath ("categories/" <> T.show eID) . withBody c)
          withFailedPut
      err `shouldSatisfy` isEntityNotFoundError

propPutCategoryCycle :: Entity Category (Front Update) -> Property
propPutCategoryCycle (Entity eID cat) =
  property $
    isJust (parent cat) ==> do
      res <-
        evalTest
          ( withPutPath ("categories/" <> T.show eID)
              . woLogger
              . withBody cat
          )
          (withGetEntities @(ID (Category (Front Update))) [eID])
      case res of
        Right r -> error $ show r
        Left err ->
          err `shouldSatisfy` isCategoryCycleError

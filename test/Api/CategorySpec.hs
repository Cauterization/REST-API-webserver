{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Api.CategorySpec where

import Api.Category
import App.Error
import App.Path
import App.Result
import App.Types
import Control.Monad
import Control.Monad.Catch
import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Data
import Data.Either
import Data.Kind
import Database.Internal qualified as Database
import Entity.Internal
import Entity.Category
import Extended.Text (Text)
import Extended.Text qualified as T
import HKD.HKD
import Mocks.Arbitrary
import Mocks.Constant
import Mocks.Entity.Category ()
import Mocks.Predicates
import Mocks.Run
import Mocks.TestMonad
import Mocks.With
import Test.Hspec
import Test.QuickCheck
import Data.Maybe
import Data.Coerce

spec :: Spec
spec = describe "PUT" $ do
  it "It actually updates category when all is ok" $ 
    property propPut
  it "Throws an appropriate error when there is no category with that IDe" $ 
    property propPutDoesntExists
  it "Throws an appropriate error when put update forms a cycle in the category tree" $ 
    property propPutCategoryCycle

propPut ::
  Category (Front Update) ->
  ID (Category Update) ->
  Property
propPut c@Category{..} eID = property $ parent /= Just (coerce eID) ==> do
  res <-
    evalTest
      (withPutPath ("categories/" <> T.show eID) . withBody c)
      id
  res `shouldBe` Right (ResText "Successfuly updated.")

propPutDoesntExists ::
  Category (Front Update) ->
  ID (Category Update) ->
  Property
propPutDoesntExists c@Category{..} eID = property $ parent /= Just (coerce eID) ==> do
  Left err <-
    evalTest
      (withPutPath ("categories/" <> T.show eID) . withBody c)
      withFailedPut
  err `shouldSatisfy` isEntityNotFoundError

propPutCategoryCycle :: Entity Category (Front Update) -> Property
propPutCategoryCycle (Entity eID cat) = property $ isJust (parent cat) ==> do
  res <- evalTest
    ( withPutPath ("categories/" <> T.show eID)
    . woLogger
    . withBody cat)
    ( withGetEntities @(ID (Category (Front Update))) [eID] )
  case res of
      Right r -> error $ show r
      Left err ->
        err `shouldSatisfy` isCategoryCycleError

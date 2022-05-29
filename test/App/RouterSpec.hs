{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module App.RouterSpec where

import App.Error
import App.Result
import App.Types
import Data.Data
import Data.Kind
import Entity.Tag
import Extended.Text (Text)
import Extended.Text qualified as T
import HKD.HKD
import Mocks.Arbitrary
import Mocks.Endpoints
import Mocks.Predicates
import Mocks.Run
import Mocks.With
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "Router" $ do
  it "When all is ok router works as expected" $
    property propRouter

  it "Throws corresponding error when there are multiple pathes with same URL" $
    property propRouterAmbiguous

  it "Throws 404 when there is no path with this URL" $
    property prop404

  it "Router middlewares works as expected" $
    property propRouterMiddleware

propRouter :: Property
propRouter = property $ do
  Right res <- evalTest (withPostPath "router ok test") id
  res `shouldBe` ResText "ok"

propRouterAmbiguous :: Property
propRouterAmbiguous = property $ do
  Left res <- evalTest (withPostPath "ambiguousPatterns test") id
  res `shouldSatisfy` isAmbiguousPatternsError

prop404 :: Text -> Property
prop404 path =
  property $
    pathDoesntExists ==> do
      Left res <- evalTest (withPostPath path) id
      res `shouldSatisfy` is404Error
  where
    pathDoesntExists = case T.splitOn "/" path of
      "tags" : _ -> False
      "users" : _ -> False
      "authors" : _ -> False
      "categories" : _ -> False
      "pictures" : _ -> False
      "articles" : _ -> False
      "drafts" : _ -> False
      "admin" : _ -> False
      _ -> True

propRouterMiddleware :: Property
propRouterMiddleware = property $ do
  Right res <- evalTest (withPostPath "middleware test") id
  res `shouldBe` ResText routerMiddlewareCorrect
  res `shouldNotBe` ResText routerMiddlewareIncorrect

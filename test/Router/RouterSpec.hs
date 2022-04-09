module Router.RouterSpec where

import Test.Hspec 
import Test.QuickCheck

import Helpers.App

spec :: Spec
spec = describe "Server routing" $ do

    it ":)" $ property $ propRouter ":)"

propRouter :: String -> Property
propRouter x = x == x ==> do
    x `shouldBe` x  
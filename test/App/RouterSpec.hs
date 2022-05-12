module App.RouterSpec where

import Helpers.App (evalTest, withPostPath, woLogger)
import Helpers.Internal (isAmbiguousPatternsError)
import Test.Hspec (Spec, describe, it, shouldSatisfy)
import Test.QuickCheck (Property, Testable (property), (==>))

spec :: Spec
spec = describe "Router works as expected" $ do
  it "Doesn't allows to have different endpoints with same URL's" $ do
    property propAmbiguousPatterns

propAmbiguousPatterns :: Property
propAmbiguousPatterns = property $ do
  Left err <-
    evalTest
      (withPostPath "ambiguous"
        . woLogger 
      )
      id
  err `shouldSatisfy` isAmbiguousPatternsError

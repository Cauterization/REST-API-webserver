module App.TypesSpec where

import App.Types ( fieldsOf )
import Data.Data (Data)
import Data.Kind (Type)
import Entity.Author ( Author )
import Entity.Category ( Category )
import Entity.Internal ( Entity )
import Entity.Tag ( Tag )
import Entity.User ( User )
import HKD.HKD ( Display, Front, Create, Update, Delete )
import Test.Hspec ( it, shouldBe, Spec, Expectation )
import Test.QuickCheck ( Testable(property) )

spec :: Spec
spec = it "Is safe to use undefinded to get names of entity fields" $ do
  property $ do
    isSafe @(Author Create)
    isSafe @(Author (Front Display))
    isSafe @(Author Update)
    isSafe @(Author (Front Update))
    isSafe @(Author Delete)
    isSafe @(User Create)
    isSafe @(User (Front Create))
    isSafe @(User Display)
    isSafe @(Entity User Display)
    isSafe @(User (Front Display))
    isSafe @(User Update)
    isSafe @(User Delete)
    isSafe @(Category Create)
    isSafe @(Category (Front Display))
    isSafe @(Category (Front Update))
    isSafe @(Category Delete)
    isSafe @(Tag Create)
    isSafe @(Tag Update)
    isSafe @(Tag (Front Update))
    isSafe @(Tag (Front Display))
    isSafe @(Tag Delete)

isSafe :: forall e. Data e => Expectation
isSafe = fieldsOf @e `shouldBe` fieldsOf @e

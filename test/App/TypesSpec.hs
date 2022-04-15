module App.TypesSpec where

import App.Types

import Control.Monad.Catch

import Data.Either
import Data.Kind (Type)

import Entity.Author
import Entity.User
import Entity.Internal
import Entity.Tag
import Entity.Category

import HKD.HKD

import Test.Hspec
import Test.QuickCheck
import Data.Data (Data)

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
        isSafe @(User TokenUpdate)
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
        isSafe @(Category (Front Display))
        isSafe @(Category (Front Display))
        isSafe @(Category (Front Display))
        isSafe @(Category (Front Display))

isSafe :: forall e. Data e => Expectation
isSafe = fieldsOf @e `shouldBe` fieldsOf @e

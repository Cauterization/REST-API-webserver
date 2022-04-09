module Api.UserSpec where

import Api.User

import Data.Aeson

import Entity.User

import GenericProps

import Test.Hspec
import Test.QuickCheck

import Helpers.User
import HKD.HKD
import App.Result
import qualified Extended.Text as T
import App.Types
import qualified Data.Map as M
import Helpers.Database
import Helpers.Monad
import Helpers.App
import qualified Data.Time as Time



spec :: Spec
spec = do
    postSpec
    
    
postSpec :: Spec
postSpec = describe "POST" $ do

    it "When all is ok it posts user into the database" 
        $ property propPostsUser 

    it "Throws error when user already in the database"
        $ property $ propPostsNotIdempotent @User "users"
        
    it "Throws error when it fails to parse request body"
        $ property $ propPostsParsingFail @User "authors"

propPostsUser ::  TestDB -> User Create -> Property
propPostsUser db u = property $ not (alreadyExists u db) ==> do
    x <- runTest 
        ( withBody (eCreateToFrontCreate u)
        . withPostPath "users")
        (withDB db)
    case x of
        (Left err, _) -> print err
        (Right (ResText token), st) -> do
            let res = filter ((== login u) . login . 
                    fromESum @(User Create)) $ filter isUser $ M.elems $ tDB st
            res `shouldBe` 
                [ UserT User
                    { firstName = firstName u
                    , lastName = lastName u
                    , login = login u
                    , token = T.tail $ T.init token
                    , password = mkHash $ password u
                    , created = Time.fromGregorian 1 2 3
                    , admin = admin u
                    }]

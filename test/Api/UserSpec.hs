module Api.UserSpec where

import Api.User

import App.Result
import App.Types
import App.Internal

import Data.Aeson
import Data.Map qualified as M

import Entity.User
import Extended.Text qualified as T

import GenericProps

import Test.Hspec
import Test.QuickCheck

import Helpers.User
import Helpers.Internal
import Helpers.Database
import Helpers.Monad
import Helpers.App

import HKD.HKD



import qualified Data.Time as Time

spec :: Spec
spec = do
    postSpec
    
postSpec :: Spec
postSpec = describe "POST" $ do

    it "When all is ok it posts user into the database" 
        $ property propPostsUser 

    it "Throws error when user already in the database"
        $ property propPostsUserAlreadyExists
        
    it "Throws error when it fails to parse request body"
        $ property $ propPostsParsingFail @User "authors"

propPostsUser ::  EMap (User Display) -> User Create -> Property
propPostsUser db u = property $ not (alreadyExists u db) ==> do
    x <- runTest 
        ( withBody (eCreateToFrontCreate u)
        . withPostPath "users")
        (withUserDatabase db)
    case x of
        (Left err, _) -> print err
        (Right (ResText token), st) -> do
            let res = filter ((== login u) . login) . M.elems $ userDB st
            res `shouldBe` 
                [ User
                { firstName = firstName u
                , lastName = lastName u
                , login = login u
                , token = T.tail $ T.init token
                , password = mkHash $ password u
                , created = Time.fromGregorian 1 2 3
                , admin = admin u
                }]

propPostsUserAlreadyExists :: User Display -> Property
propPostsUserAlreadyExists u = property $ do
    Left err <- evalTest 
        ( withBody u'
        . withPostPath "users")
        ( withUserDatabase (M.fromList [(1, u)]) 
        )
    err `shouldBe` AlreadyExists ""
  where
    u' = User
        { firstName = firstName u
        , lastName  = lastName u
        , login     = login u
        , password  = password u
        , admin     = admin u
        } :: User Create
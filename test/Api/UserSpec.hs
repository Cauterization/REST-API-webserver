module Api.UserSpec where

import Api.User

import App.Result
import App.Types
import App.Internal

import Data.Aeson
import Data.List (sort)
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
    getSpec
    deleteSpec
    
postSpec :: Spec
postSpec = describe "POST" $ do

    it "When all is ok it posts user into the database" 
        $ property propPostsUser 

    it "Throws error when user already in the database"
        $ property propPostsUserAlreadyExists
        
    it "Throws error when it fails to parse request body"
        $ property $ propPostsParsingFail @User "authors"

getSpec :: Spec
getSpec = describe "GET" $ do

    it "Allows to get user by its own token"
        $ property propGetUser

    it "Throws error when there is no token provided"
        $ property propGetUserNoToken

    it "Throws error when wrong token is provided"
        $ property propGetUserWrongToken

deleteSpec :: Spec
deleteSpec = describe "DELETE" $ do

    it "actually deletes user from database"
        $ property $ propDeleteEntity @User "users"

    it "Throws error when user with this ID doesn't exists"
        $ property $ propDeleteEntityDoesntExists @User "users"

propPostsUser ::  EMap (User Display) -> User Create -> Property
propPostsUser db u = property $ not (alreadyExists u db) ==> do
    (Right (ResText token), st) <- runTest 
        ( withBody (eCreateToFrontCreate u)
        . withPostPath "users")
        ( withUserDatabase db )
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
    err `shouldSatisfy` isAlreadyExistsError
  where
    u' = User
        { firstName = firstName u
        , lastName  = lastName u
        , login     = login u
        , password  = password u
        , admin     = admin u
        } :: User Create

propGetUser :: EMap (User Display) -> Property 
propGetUser db = property $ conditions ==> do
    Right (ResJSON j) <- evalTest 
        ( withToken t
        . withGetPath "users/me" )
        ( withUserDatabase db )
    eitherDecode j `shouldBe` 
        Right (userDisplayToUserFrontDisplay $ minimum $ M.elems db)
  where
    t = token $ minimum $ M.elems db
    conditions = not (M.null db) && nonRepetitiveToken
    nonRepetitiveToken = (== 1) . length . filter (\User{..} -> token == t) $ M.elems db

propGetUserNoToken :: EMap (User Display) -> Property 
propGetUserNoToken db = property $ not (M.null db) ==> do
    Left err <- evalTest 
        ( withGetPath "users/me" )
        ( withUserDatabase db )
    err `shouldSatisfy` isUnathorizedError 

propGetUserWrongToken :: Token -> EMap (User Display) -> Property 
propGetUserWrongToken t db = property $ conditions ==> do
    Left err <- evalTest 
        ( withToken t
        . withGetPath "users/me" )
        ( withUserDatabase db )
    err `shouldSatisfy` isEntityNotFoundError 
  where
    conditions = not (M.null db) && wrongToken
    wrongToken = not . any (\User{..} -> token == t) $ M.elems db
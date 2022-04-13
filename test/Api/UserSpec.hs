{-# LANGUAGE ViewPatterns #-}

module Api.UserSpec where

import Api.User

import App.Result
import App.Types
import App.Internal

import Data.Aeson
import Data.List (sort)
import Data.Map qualified as M
import Data.Time qualified as Time

import Entity.User
import Extended.Text qualified as T

import Helpers.User
import Helpers.Entity
import Helpers.Internal
import Helpers.Database
import Helpers.Monad
import Helpers.App
import Helpers.GenericProps

import HKD.HKD

import Test.Hspec
import Test.QuickCheck
import Data.Either
import Data.String (fromString)

spec :: Spec
spec = do
    pure ()
    -- postSpec
    -- getSpec
    -- deleteSpec
    -- authSpec
    
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

    it "Actually deletes user from database"
        $ property $ propDeleteEntity @User "users"

    it "Throws error when user with this ID doesn't exists"
        $ property $ propDeleteEntityDoesntExists @User "users"

authSpec :: Spec
authSpec = describe "User authentication" $ do

    it "When all is ok it gives new token to user and updates token in the database"
        $ property propAuthUser

    it "Throws error when user with this login doesn't exists"
        $ property propAuthUserDoesntExists

    it "Throws error when invalid request body is provided"
        $ property propAuthParsingFail

propPostsUser :: TDB User -> User Create -> Property
propPostsUser db u = property $ not (alreadyExists u db) ==> do
    (Right (ResJSON j), st) <- runTest 
        ( withBody (eCreateToFrontCreate u)
        . withPostPath "users")
        ( withDatabase @User db )
    let Right (userID, token) = eitherDecode j
    M.lookup userID (userDB st) `shouldBe` 
        Just User
        { firstName = firstName u
        , lastName = lastName u
        , login = login u
        , token = token
        , password = mkHash $ password u
        , created = Time.fromGregorian 1 2 3
        , admin = admin u
        }

propPostsUserAlreadyExists :: User Display -> Property
propPostsUserAlreadyExists u = property $ do
    Left err <- evalTest 
        ( withBody u'
        . withPostPath "users")
        ( withDatabase @User (M.fromList [(1, u)]) 
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

propGetUser :: TDB User -> Property 
propGetUser db = property $ conditions ==> do
    Right (ResJSON j) <- evalTest 
        ( withToken t
        . withGetPath "users/me" )
        ( withDatabase @User db )
    eitherDecode j `shouldBe` 
        Right (userDisplayToUserFrontDisplay $ minimum $ M.elems db)
  where
    t = token $ minimum $ M.elems db
    conditions = not (M.null db) && nonRepetitiveToken
    nonRepetitiveToken = (== 1) . length . filter (\User{..} -> token == t) $ M.elems db

propGetUserNoToken :: TDB User -> Property 
propGetUserNoToken db = property $ not (M.null db) ==> do
    Left err <- evalTest 
        ( withGetPath "users/me" )
        ( withDatabase @User db )
    err `shouldSatisfy` isUnathorizedError 

propGetUserWrongToken :: Token -> TDB User -> Property 
propGetUserWrongToken t db = property $ conditions ==> do
    Left err <- evalTest 
        ( withToken t
        . withGetPath "users/me" )
        ( withDatabase @User db )
    err `shouldSatisfy` isEntityNotFoundError 
  where
    conditions = not (M.null db) && wrongToken
    wrongToken = not . any (\User{..} -> token == t) $ M.elems db

propAuthUser :: User Display -> ID (User Display) -> TDB User -> Property
propAuthUser user uID (M.filter ((/= login user) . login) -> db) 
    = property $ token user /= "super unique token" ==> do
        (Right (ResText newToken), st) <- runTest 
            ( withBody auth 
            . withPostPath "auth" ) 
            ( withDatabase $ M.insert uID dbUser db )
        newToken `shouldNotBe` token user
        let Just user' = M.lookup uID $ userDB st
        token user' `shouldNotBe` newToken
  where
    dbUser = (\User{..} -> User{password = mkHash password, .. }) user
    auth = User
        { firstName = Nothing
        , lastName = Nothing
        , login = login user
        , token = Nothing
        , password = password user
        , created = Nothing
        , admin = Nothing
        } :: User Auth

propAuthUserDoesntExists :: User Display -> ID (User Display) -> TDB User -> Property
propAuthUserDoesntExists user uID (M.filter ((/= login user) . login) -> db) 
    = property $ do
        Left err <- evalTest 
            ( withBody auth 
            . withPostPath "auth" ) 
            ( withDatabase db )
        err `shouldSatisfy` isEntityNotFoundError
  where
    auth = User
        { firstName = Nothing
        , lastName = Nothing
        , login = login user
        , token = Nothing
        , password = password user
        , created = Nothing
        , admin = Nothing
        } :: User Auth

propAuthParsingFail :: TDB User -> Value -> Property
propAuthParsingFail db obj
    = property $ conditions ==> do
        Left err <- evalTest 
            ( withBody obj 
            . withPostPath "auth" ) 
            ( withDatabase db )
        err `shouldSatisfy` isParsingError 
  where
    conditions = isLeft $ eitherDecode @(User Auth) $ fromString $ show obj


{-# LANGUAGE ViewPatterns #-}

module Api.UserSpec where

import Api.User

import App.Result
import App.Types
import App.Internal

import Data.Aeson
import Data.List (sort)
import Data.IntMap qualified as IM
import Data.Time qualified as Time

import Entity.User
import Entity.Internal
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
    postSpec
    getSpec
    deleteSpec
    authSpec
    
postSpec :: Spec
postSpec = describe "POST" $ do

    it "When all is ok it posts user into the database" 
        $ property propPostsUser 

    it "Throws error when user already in the database"
        $ property propPostsUserAlreadyExists
        
    it "Throws error when it fails to parse request body"
        $ property $ propPostsParsingFail @User "users"

propPostsUser :: TDB User -> User (Front Create) -> Property
propPostsUser db u = property $ not (alreadyExists u db) ==> do
    (Right (ResJSON j), st) <- runTest 
        ( withBody u
        . withPostPath "users")
        ( withDatabase @User db )
    let Right (userID, token) = eitherDecode j
    IM.lookup userID (_tsUserDB st) `shouldBe` 
        Just User
        { firstName  = firstName u
        , lastName   = lastName u
        , login      = login u
        , token      = token
        , password   = mkHash $ password u
        , registered = testDate
        , admin      = admin u
        }

propPostsUserAlreadyExists :: User Display -> Property
propPostsUserAlreadyExists u = property $ do
    Left err <- evalTest 
        ( withBody u'
        . withPostPath "users")
        ( withDatabase @User (IM.fromList [(1, u)]) 
        )
    err `shouldSatisfy` isAlreadyExistsError
  where
    u' = User
        { firstName  = firstName u
        , lastName   = lastName u
        , login      = login u
        , password   = password u
        , admin      = admin u
        , token      = Nothing
        , registered = Nothing
        } :: User (Front Create)

getSpec :: Spec
getSpec = describe "GET" $ do

    it "Allows to get user by its own token"
        $ property propGetUser

    it "Throws error when there is no token provided"
        $ property propGetUserNoToken

    it "Throws error when wrong token is provided"
        $ property propGetUserWrongToken

propGetUser :: TDB User -> Property 
propGetUser db = property $ conditions ==> do
    Right (ResJSON j) <- evalTest 
        ( withToken t
        . withGetPath "users/me" )
        ( withDatabase @User db )
    let [(eID, u)] = IM.toList $ IM.filter ((== t) . token) db
    j `shouldBe` 
        encode @(Entity User (Front Display)) (Entity (ID eID) $ fromDisplay u)
  where
    t = token $ minimum $ IM.elems db
    conditions = not (IM.null db) && nonRepetitiveToken
    nonRepetitiveToken = (== 1) . length . filter (\User{..} -> token == t) $ IM.elems db

propGetUserNoToken :: TDB User -> Property 
propGetUserNoToken db = property $ not (IM.null db) ==> do
    Left err <- evalTest 
        ( withGetPath "users/me" )
        ( withDatabase @User db )
    err `shouldSatisfy` isUnathorizedError 

propGetUserWrongToken :: Token -> TDB User -> Property 
propGetUserWrongToken t db = property $ not (IM.null db) && wrongToken ==> do
    Left err <- evalTest 
        ( withToken t
        . withGetPath "users/me" )
        ( withDatabase @User db )
    err `shouldSatisfy` isEntityNotFoundError 
  where
    wrongToken = not . any (\User{..} -> token == t) $ IM.elems db

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

propAuthUser :: User Display -> ID (User Display) -> TDB User -> Property
propAuthUser user (ID userID) unpreaparedDB
    = property $ token user /= testToken ==> do
        (Right (ResText newToken), st) <- runTest 
            ( withBody auth 
            . withPostPath "auth" ) 
            ( withDatabase @User db )
        let Just user' = IM.lookup userID $ _tsUserDB st
        newToken `shouldNotBe` token user
        T.show (token user') `shouldBe` newToken
  where
    db = let p u = (login u /= login user && token u /= token user)
         in IM.insert userID dbUser $ IM.filter p unpreaparedDB
    dbUser = (\User{..} -> User{password = mkHash password, .. }) user
    auth = User
        { firstName = Nothing
        , lastName = Nothing
        , login = login user
        , token = Nothing
        , password = password user
        , registered = Nothing
        , admin = Nothing
        } :: User Auth

propAuthUserDoesntExists :: User Display -> ID (User Display) -> TDB User -> Property
propAuthUserDoesntExists user uID (IM.filter ((/= login user) . login) -> db) 
    = property $ do
        Left err <- evalTest 
            ( withBody auth 
            . withPostPath "auth" ) 
            ( withDatabase @User db )
        err `shouldSatisfy` isEntityNotFoundError
  where
    auth = User
        { firstName = Nothing
        , lastName = Nothing
        , login = login user
        , token = Nothing
        , password = password user
        , registered = Nothing
        , admin = Nothing
        } :: User Auth

propAuthParsingFail :: TDB User -> Value -> Property
propAuthParsingFail db obj
    = property $ conditions ==> do
        Left err <- evalTest 
            ( withBody obj 
            . withPostPath "auth" ) 
            ( withDatabase @User db )
        err `shouldSatisfy` isParsingError 
  where
    conditions = isLeft $ eitherDecode @(User Auth) $ fromString $ show obj




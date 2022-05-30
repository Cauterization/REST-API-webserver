{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Api.UserSpec where

import App.Error
import App.Path
import App.Result
import App.Types
import Control.Monad
import Control.Monad.Catch
import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Data
import Data.Either
import Data.Kind
import Database.Internal qualified as Database
import Entity.Internal
import Entity.User
import Extended.Text (Text)
import Extended.Text qualified as T
import HKD.HKD
import Mocks.Arbitrary
import Mocks.Constant
import Mocks.Predicates
import Mocks.Run
import Mocks.TestMonad
import Mocks.With
import Test.Hspec
import Test.QuickCheck
import Api.User


spec :: Spec
spec = do
  describe "POST" $ do
    it "Actually posts user into database when all is ok" $ 
      property propPostUser
  describe "GET" $ do
    it "Allows to get user by its own token" $ 
      property propGetUser
    it "Throws an appropriate error when no token is provided" $ 
      property propGetUserNoToken
    it "Throws an appropriate error when wrong token is provided" $ 
      property propGetUserWrongToken
  describe "Auth" $ do
    it "Gives new token to user when all is ok" $ 
      property propAuthUser
    it "Throws an appropriate error when wrong password is provided" $ 
      property propAuthUserWrongPassword
    it "Throws an appropriate error when request body is unparsable" $ 
      property propAuthUserUnparsable
    it "Throws an appropriate error when user with this login doesn't exists" $ 
      property propAuthUserDoesntExists

propPostUser :: User (Front Create) -> Property
propPostUser user = property $ do
    res <- evalTest
      (withPostPath "users" . withBody user)
      id
    res `shouldBe` Right (ResJSON $ encode (defaultPostResult, testTokenConstant))

propGetUser :: Token -> Entity User (Front Display) -> Property
propGetUser t user = property $ do
    Right res <- evalTest
      (withGetPath "users/me" . withToken t)
      (withGetEntities @(Entity User (Front Display)) [user])
    res `shouldBe` ResJSON (encode  user)

propGetUserNoToken :: Property
propGetUserNoToken = property $ do
    Left err <- evalTest
      (withGetPath "users/me")
      id
    err `shouldSatisfy` isUnathorizedError

propGetUserWrongToken :: Token -> Property
propGetUserWrongToken t = property $ do
    Left err <- evalTest
      (withGetPath "users/me" . withToken t)
      id
    err `shouldSatisfy` isEntityNotFoundError

propAuthUser :: Entity User Display -> Property    
propAuthUser (Entity eID User{..}) = property $ token /= testTokenConstant ==> do
  let auth = User
       { firstName = Nothing
       , lastName = Nothing
       , login = login 
       , token = Nothing
       , password = password 
       , registered = Nothing
       , admin = Nothing
       } :: User Auth
  Right (ResText newToken) <- evalTest
    (withPostPath "auth" . withBody auth)
    (withGetEntities @(Entity User Display) [Entity eID User{password = mkHash password, ..}])
  newToken `shouldBe` testTokenConstant
  newToken `shouldNotBe` token

propAuthUserWrongPassword :: Text -> Entity User Display -> Property    
propAuthUserWrongPassword wrong (Entity eID User{..}) = property $ wrong /= password ==> do
  let auth = User
       { firstName = Nothing
       , lastName = Nothing
       , login = login 
       , token = Nothing
       , password = wrong 
       , registered = Nothing
       , admin = Nothing
       } :: User Auth
  res <- evalTest
    (withPostPath "auth" . withBody auth)
    (withGetEntities @(Entity User Display) [Entity eID User{password = mkHash password, ..}])
  case res of
      Left err -> 
         err `shouldSatisfy` isWrongPasswordError
      Right r -> print r

propAuthUserUnparsable :: BL.ByteString -> Property    
propAuthUserUnparsable bl = property $ isLeft (eitherDecode @(User Auth) bl) ==> do
  Left err <- evalTest
    (withPostPath "auth" . withBLBody bl)
    id
  err `shouldSatisfy` isParsingError

propAuthUserDoesntExists :: User Auth -> Property    
propAuthUserDoesntExists auth = property $ do
  Left err <- evalTest
    (withPostPath "auth" . withBody auth)
    id
  err `shouldSatisfy` isEntityNotFoundError
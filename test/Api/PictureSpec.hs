{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Api.PictureSpec where

import App.Error
import App.Path
import App.Result
import App.Types
import Control.Monad
import Control.Monad.Catch
import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Char
import Data.Data
import Data.Either
import Data.Kind
import Database.Internal qualified as Database
import Entity.Internal
import Entity.Picture
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
    it "Actually posts picture into database when all is ok" $
      property propPostsPicture

    it "Throws an appropriate error when picture has unknown format" $
      property propPostsPictureUnknownFormat
  describe "GET" $ do
    it "Allows to get picture from database when all is ok" $
      property propGetPicture

propPostsPicture :: Picture (Front Create) -> Property
propPostsPicture Picture{..} = property $ do
  res <- evalTest
    ( withPostPath "pictures"
    . withContentType ("image/" <> T.show format)
    . withBLBody picture
    )
    id
  res `shouldBe` Right (ResText $ T.show defaultPostResult)

propPostsPictureUnknownFormat :: Picture (Front Create) -> Text -> Property
propPostsPictureUnknownFormat Picture{..} wrongFormat = property $ formatIsWrong ==> do
  Left err <- evalTest
    ( withPostPath "pictures"
    . withContentType ("image/" <> T.show wrongFormat)
    . withBLBody picture
    )
    id
  err `shouldSatisfy` isRequestHeadersError
  where
    formatIsWrong = T.map toLower wrongFormat `notElem` ["jpeg", "gif", "png"] 

propGetPicture :: Entity Picture (Front Display) -> Property
propGetPicture (Entity eID p) = property $ do
  res <- evalTest
    (withGetPath $ "pictures/" <> T.show eID)
    (withGetEntities @(Picture (Front Display)) [p])
  res `shouldBe` Right (ResPicture p)

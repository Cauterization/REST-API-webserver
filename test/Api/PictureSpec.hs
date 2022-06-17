{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module Api.PictureSpec where

import App.Result (AppResult (ResPicture, ResText))
import Data.Char (toLower)
import Entity.Internal (Entity (Entity))
import Entity.Picture (Picture (..))
import Extended.Text (Text)
import Extended.Text qualified as T
import HKD.HKD (Create, Display, Front)
import Mocks.Predicates (isRequestHeadersError)
import Mocks.Run (evalTest)
import Mocks.TestMonad
  ( TestEntity (withGetEntities),
    defaultPostResult,
  )
import Mocks.With
  ( withBLBody,
    withContentType,
    withGetPath,
    withPostPath,
  )
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.QuickCheck (Property, Testable (property), (==>))

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
propPostsPicture Picture {..} = property $ do
  res <-
    evalTest
      ( withPostPath "pictures"
          . withContentType ("image/" <> T.show format)
          . withBLBody picture
      )
      id
  res `shouldBe` Right (ResText $ T.show defaultPostResult)

propPostsPictureUnknownFormat :: Picture (Front Create) -> Text -> Property
propPostsPictureUnknownFormat Picture {..} wrongFormat =
  property $
    formatIsWrong ==> do
      Left err <-
        evalTest
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
  res <-
    evalTest
      (withGetPath $ "pictures/" <> T.show eID)
      (withGetEntities @(Picture (Front Display)) [p])
  res `shouldBe` Right (ResPicture p)

{-# LANGUAGE ViewPatterns #-}

module Api.PictureSpec where

import App.Types
import App.Result

import Control.Lens

import Data.Aeson
import Data.Either
import Data.IntMap qualified as IM
import Data.Coerce
import Data.Kind (Type)
import Data.List
import Data.Char

import Entity.Picture
import Entity.User
import Entity.Internal
import Extended.Text qualified as T
import Extended.Text (Text)
import Extended.Text qualified as T


-- import Helpers.Picture
import Helpers.Internal
import Helpers.Entity
import Helpers.GenericProps
import Helpers.Picture
import Test.Hspec 
import Test.QuickCheck
import Helpers.Monad
import Helpers.App
import Helpers.Database
import HKD.HKD
import Data.Data (Typeable)

import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
    pure ()
    postSpec
    getSpec
    deleteSpec
    
postSpec :: Spec
postSpec = describe "POST" $ do

    it "When all is ok it posts tag into the database" 
        $ property $ propPostsPicture

    it "Throws error when picture has unknown format"
        $ property $ propPostsPictureUnknownFormat

propPostsPicture :: TDB Picture -> Picture (Front Create) -> Property
propPostsPicture db Picture{..} = property $ do
    res <- runTest
        ( withBLBody picture 
        . withContentType ("image/" <> T.show format)
        . withPostPath "pictures")
        ( withDatabase @Picture db )
    case res of
        (Left err, _ ) -> error $ show err
        (Right (ResText pictureIDtext), st) -> do
            let Right pictureID = T.read pictureIDtext
            IM.lookup pictureID (_tsPictureDB st) `shouldBe` Just Picture{..}

propPostsPictureUnknownFormat :: TDB Picture -> Text -> Picture (Front Create) -> Property
propPostsPictureUnknownFormat db wrongFormat Picture{..} = property $ formatIsWrong ==> do
    Left err <- evalTest
        ( withBLBody picture 
        . withContentType ("image/" <> T.show wrongFormat)
        . withPostPath "pictures")
        ( withDatabase @Picture db )
    err `shouldSatisfy` isRequestHeadersError
  where
    formatIsWrong = T.map toLower wrongFormat `notElem` ["jpeg", "gif", "png"]

getSpec :: Spec
getSpec = describe "GET" $ do

    it "Allows to get picture by its own ID"
        $ property propGetPicture

    it "Throws error when picture with this ID doesn't exists"
        $ property $ propGetEntityDoesntExists @Picture "pictures"


propGetPicture :: TDB Picture -> ID (Picture Display) -> Property 
propGetPicture db (ID pictureID) = property $ pictureID `elem` IM.keys db ==> do
    res <- evalTest 
        ( withGetPath $ "pictures/" <> T.show pictureID )
        ( withDatabase @Picture db )
    case res of
        Right (ResPicture p) -> Just p `shouldBe` (fromDisplay <$> IM.lookup pictureID db)
        Left err -> error $ show err

deleteSpec :: Spec
deleteSpec = describe "DELETE" $ do

    it "Actually deletes picture from database"
        $ property $ propDeleteEntity @Picture "pictures"

    it "Throws error when picture with this ID doesn't exists"
        $ property $ propDeleteEntityDoesntExists @Picture "pictures"

    
module Api.PictureSpec where

import App.Result ( AppResult(ResPicture, ResText) )
import App.Types ( ID(ID) )
import Data.Char ( toLower )
import Data.Data (Typeable)
import Data.IntMap qualified as IM
import Data.Kind (Type)
import Entity.Picture ( Picture(..) )
import Extended.Text (Text)
import Extended.Text qualified as T
import HKD.HKD ( Display, Front, Create )
import Helpers.App
    ( evalTest,
      runTest,
      withBLBody,
      withContentType,
      withGetPath,
      withPostPath )
import Helpers.Database ( withDatabase, TestEntity(fromDisplay) )
import Helpers.GenericProps
    ( propDeleteEntity,
      propDeleteEntityDoesntExists,
      propGetEntityDoesntExists )
import Helpers.Internal ( isRequestHeadersError )
import Helpers.Monad ( TDB, TestState(_tsPictureDB) )
import Helpers.Picture ()
import Test.Hspec ( Spec, describe, it, shouldBe, shouldSatisfy )
import Test.QuickCheck ( (==>), Property, Testable(property) )

spec :: Spec
spec = do
  postSpec
  getSpec
  deleteSpec

postSpec :: Spec
postSpec = describe "POST" $ do
  it "When all is ok it posts picture into the database" $
    property propPostsPicture

  it "Throws error when picture has unknown format" $
    property propPostsPictureUnknownFormat

propPostsPicture :: TDB Picture -> Picture (Front Create) -> Property
propPostsPicture db Picture {..} = property $ do
  (Right (ResText pictureIDtext), st) <-
    runTest
      ( withBLBody picture
          . withContentType ("image/" <> T.show format)
          . withPostPath "pictures"
      )
      (withDatabase @Picture db)
  let Right pictureID = T.read pictureIDtext
  IM.lookup pictureID (_tsPictureDB st) `shouldBe` Just Picture {..}

propPostsPictureUnknownFormat :: TDB Picture -> Text -> Picture (Front Create) -> Property
propPostsPictureUnknownFormat db wrongFormat Picture {..} =
  property $
    formatIsWrong ==> do
      Left err <-
        evalTest
          ( withBLBody picture
              . withContentType ("image/" <> T.show wrongFormat)
              . withPostPath "pictures"
          )
          (withDatabase @Picture db)
      err `shouldSatisfy` isRequestHeadersError
  where
    formatIsWrong = T.map toLower wrongFormat `notElem` ["jpeg", "gif", "png"]

getSpec :: Spec
getSpec = describe "GET" $ do
  it "Allows to get picture by its own ID" $
    property propGetPicture

  it "Throws error when picture with this ID doesn't exists" $
    property $ propGetEntityDoesntExists @Picture "pictures"

propGetPicture :: TDB Picture -> ID (Picture Display) -> Property
propGetPicture db (ID pictureID) =
  property $
    pictureID `elem` IM.keys db ==> do
      res <-
        evalTest
          (withGetPath $ "pictures/" <> T.show pictureID)
          (withDatabase @Picture db)
      case res of
        Right (ResPicture p) -> Just p `shouldBe` (fromDisplay <$> IM.lookup pictureID db)
        Left err -> error $ show err

deleteSpec :: Spec
deleteSpec = describe "DELETE" $ do
  it "Actually deletes picture from database" $
    property $ propDeleteEntity @Picture "pictures"

  it "Throws error when picture with this ID doesn't exists" $
    property $ propDeleteEntityDoesntExists @Picture "pictures"

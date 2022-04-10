module Api.AuthorSpec where

import App.Result

import Data.Aeson
import Data.Map qualified as M
import Data.Coerce
import Data.List
import Data.Either

import Entity.Author
import Entity.User
import Entity.Internal
import Extended.Text qualified as T

import Helpers.Author
import Helpers.Internal
import Helpers.Entity
import Helpers.GenericProps
import Helpers.Update
import Test.Hspec 
import Test.QuickCheck
import Helpers.Monad
import Helpers.App
import Helpers.Database

import HKD.HKD
import Extended.Text (Text)
import Data.Kind (Type)
import App.Types


spec :: Spec
spec = do
    postSpec
    getSpec
    putSpec
    deleteSpec
    
postSpec :: Spec
postSpec = describe "POST" $ do

    it "When all is ok it posts author into the database" 
        $ property propPostsAuthor 

    it "Throws error when author already in the database"
        $ property propPostsAuthorAlreadyExists
        
    it "Throws error when it fails to parse request body"
        $ property $ propPostsParsingFail @Author "authors"

getSpec :: Spec
getSpec = describe "GET" $ do

    context "many" $ do

        it "When all is ok it gets list of authors"
            $ property $ propGetEntities @Author "authors"

        it "This list is paginated"
            $ property $ propGetEntitiesIsPaginated @Author "authors"

        it "Allows to get various pages of this list"
            $ property $ propGetEntitiesWithPage @Author "authors"

    context "single" $ do

        it "When all is ok it allows to get an author by its own ID"
            $ property $ propGetEntity @Author "authors"

        it "Throws error when author with this ID doesn't exists"
            $ property $ propGetEntityDoesntExists @Author "authors"

putSpec :: Spec
putSpec = describe "PUT" $ do

    it "Actually change author "
        $ property $ propPutEntity @Author "authors"

    it "Throws error when author with this ID doesn't exists"
        $ property $ propPutEntityDoesntExists @Author "authors"

    it "Throws error when it fails to parse request body"
        $ property $ propPutParsingFail @Author "authors"

deleteSpec :: Spec
deleteSpec = describe "DELETE" $ do

    it "Actually deletes author from database"
        $ property $ propDeleteEntity @Author "authors"

    it "Throws error when author with this ID doesn't exists"
        $ property $ propDeleteEntityDoesntExists @Author "authors"

propPostsAuthor :: EMap (Author Display) -> User Display -> Author Create -> Property
propPostsAuthor db u a = property $ not (alreadyExists a db) ==> do
    (Right (ResText aIDtext), st) <- runTest 
        ( withBody (eCreateToFrontCreate a)
        . withPostPath "authors")
        ( withDatabase @User (M.fromList [(coerce $ user a, u)]) 
        . withDatabase @Author db)
    let Right aID = T.read aIDtext
    let res = M.lookup aID $ authorDB st
    fmap fromDisplay res `shouldBe` Just a

propPostsAuthorAlreadyExists :: 
    EMap (Author Display) -> EMap (Author Display) -> EMap (Author Display) 
    -> User Display -> Author Create -> Property
propPostsAuthorAlreadyExists db1 db2 db3 u a = property $ alreadyExists a db ==> do
    res <- evalTest 
        ( withBody (eCreateToFrontCreate a)
        . withPostPath "authors")
        ( withDatabase @User (M.fromList [(coerce $ user a, u)]) 
        . withDatabase @Author db)
    isLeft res `shouldBe` True
  where
    db = db1 <> db2 <> db3




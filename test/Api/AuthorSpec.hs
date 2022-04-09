module Api.AuthorSpec where

import App.Result

import Data.Map qualified as M
import Data.Coerce
import Data.Either

import Entity.Author
import Entity.User
import Entity.Internal
import Extended.Text qualified as T

import GenericProps

import Helpers.Author
import Helpers.Internal

import Test.Hspec 
import Test.QuickCheck
import Helpers.Monad
import Helpers.App
import Helpers.Database

import HKD.HKD


spec :: Spec
spec = do
    postSpec
    -- getSpec
    
postSpec :: Spec
postSpec = describe "POST" $ do

    it "When all is ok it posts author into the database" 
        $ property propPostsAuthor 

    it "Throws error when author already in the database"
        $ property $ propPostsAuthorAlreadyExists
        
    it "Throws error when it fails to parse request body"
        $ property $ propPostsParsingFail @Author "authors"

-- getSpec :: Spec
-- getSpec = describe "GET" $ do

--     it "When all is ok it gets list of authors"
--         $ property $ propGetEntities

-- propGetEntities :: TestDB -> Property
-- propGetEntities db = property $ do
--     Right x <- evalTest (withGetPath "authors") (withDB db)
--     x `shouldBe` _ (map (fromESum @(Author (Front Display))) (filter isAuthor $ M.elems db))


propPostsAuthor :: EMap (Author Display) -> User Display -> Author Create -> Property
propPostsAuthor db u a = property $ not (alreadyExists a db) ==> do
    (Right (ResText aIDtext), st) <- runTest 
        ( withBody (eCreateToFrontCreate a)
        . withPostPath "authors")
        ( withUserDatabase (M.fromList [(coerce $ user a, u)]) 
        . withAuthorDatabase db)
    let Right aID = T.read aIDtext
    let res = M.lookup aID $ authorDB st
    fmap fromDisplay res `shouldBe` Just a

propPostsAuthorAlreadyExists :: EMap (Author Display) -> User Display -> Author Create -> Property
propPostsAuthorAlreadyExists db u a = property $ alreadyExists a db ==> do
    res <- evalTest 
        ( withBody (eCreateToFrontCreate a)
        . withPostPath "authors")
        ( withUserDatabase (M.fromList [(coerce $ user a, u)]) 
        . withAuthorDatabase db)
    isLeft res `shouldBe` True
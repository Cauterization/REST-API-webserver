module Api.AuthorSpec where

import Entity.Author

import GenericProps

import Helpers.Author

import Test.Hspec 
import Test.QuickCheck


spec :: Spec
spec = do
    postSpec
    
    
postSpec :: Spec
postSpec = describe "POST" $ do

    it "When all is ok it posts author into the database" 
        $ property $ propPostsEntity @Author "authors"

    it "Throws error when author already in the database"
        $ property $ propPostsNotIdempotent @Author "authors"
        
    it "Throws error when it fails to parse request body"
        $ property $ propPostsParsingFail @Author "authors"


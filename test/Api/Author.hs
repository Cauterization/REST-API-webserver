module Router.RouterSpec where

import Test.Hspec 
import Test.QuickCheck
import Entity.Author

import HKD.HKD

spec :: Spec
spec = describe "Author API" $ do

    it "When all is ok it posts author inti the database" 
        $ property propPostsAuthor

propRouter :: String -> Property
propRouter x = x == x ==> do
    x `shouldBe` x  


propPostsAuthor :: Author (Front Create) -> Property
propPostsAuthor = undefined

-- postAuthor :: forall m.
--     ( Application m
--     , Postable m Author
--     , Impure m
--     ) => Endpoint m
-- postAuthor _ = do
--     Author{..} <- decodedBody @(Author (Front Create))
--     Logger.info "Attempt to post author"
--     Database.postEntity (Author (coerce user) description ) >>= text
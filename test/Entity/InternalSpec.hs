module Entity.InternalSpec where

import Data.Aeson
import Data.Data

import Entity.Internal
import Entity.User

import HKD.HKD

import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "Aeson entity instances" $ do

    pure ()

    -- it "fromJSON . toJSON == id" $ property $ do
    --     propFromTo @User @(Front Display)


propFromTo :: forall e a. 
    ( Arbitrary (Entity e a)
    , Show     (e a)
    , Eq       (Entity e a)
    , ToJSON   (e a)
    , FromJSON (e a)
    , Typeable e
    ) => Entity e a -> Property
propFromTo e = property $ do
    decode (encode e) `shouldBe` Just e


-- getSpec :: Spec
-- getSpec = describe "GET" $ do

--     it "Allows to get user by its own token"
--         $ property propGetUser

--     it "Throws error when there is no token provided"
--         $ property propGetUserNoToken

--     it "Throws error when wrong token is provided"
--         $ property propGetUserWrongToken

-- propGetUser :: TDB User -> Property 
-- propGetUser db = property $ conditions ==> do
--     Right (ResJSON j) <- evalTest 
--         ( withToken t
--         . withGetPath "users/me" )
--         ( withDatabase @User db )
--     let [(eID, u)] = M.toList $ M.filter ((== t) . token) db
--     j `shouldBe` 
--         encode @(Entity User (Front Display)) (Entity (ID eID) $ fromDisplay u)

--   where
--     t = token $ minimum $ M.elems db
--     conditions = not (M.null db) && nonRepetitiveToken
--     nonRepetitiveToken = (== 1) . length . filter (\User{..} -> token == t) $ M.elems db
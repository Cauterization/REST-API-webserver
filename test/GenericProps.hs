{-# LANGUAGE ViewPatterns #-}
module GenericProps where

import Data.Aeson
import Data.ByteString.Lazy qualified as BL 
import Data.Map qualified as M
import Data.Maybe

import Entity.Internal
import Extended.Text (Text)
import Extended.Text qualified as T


import App.Types
import App.Result
import App.Internal hiding (decode)

import HKD.HKD

import Helpers.App
import Helpers.Database
import Helpers.Internal
import Helpers.Monad

import Test.Hspec
import Test.QuickCheck
import Data.String

propPostsEntity :: forall e. 
    ( ToJSON (e Create) 
    , Show (e Create) 
    , Eq (e Create) 
    , FromJSON (e (Front Create))
    , ToJSON (e (Front Create))
    , FromRowOfT (ID (e Create))
    , ToRowOfT (e Create)
    , ToRowOfT (e Display)
    , ToDisplay (e Display) ~ e Display
    , DBOf (e Create) ~ M.Map (ID (e Display)) (e Display)
    ) => Text -> DBOf (e Create) -> e Create -> Property
propPostsEntity path db e = property $ not (alreadyExists e db) ==> do
    x <- runTest 
        ( withBody (eCreateToFrontCreate e)
        . withPostPath path)
        ( withDatabase @(e Create) db )
    case x of
        (Right (ResText t), st) -> do
            let Right eID = T.read @(ID (e Display)) t
            fromDisplay <$> M.lookup eID (dbFromTestState @(e Display) st) 
                `shouldBe` Just e
        (Left err, _) -> print err

eCreateToFrontCreate :: forall e.
    ( ToJSON (e Create)
    , FromJSON (e (Front Create))
    ) => e Create -> e (Front Create)
eCreateToFrontCreate ec = fromJust $ decode @(e (Front Create)) $ encode ec

propPostsParsingFail :: forall e. 
    ( FromJSON (e (Front Create))
    ) => Text -> Value -> Property
propPostsParsingFail path (fromString . show -> obj) = 
    property $ isNothing (decode @(e (Front Create)) obj) ==> do
        Left res <- evalTest 
            ( withBLBody obj
            . withPostPath path )
            id
        res `shouldSatisfy` isParsingError
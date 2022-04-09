{-# LANGUAGE ViewPatterns #-}
module GenericProps where

import Data.Aeson
import Data.ByteString.Lazy qualified as BL 
import Data.Map qualified as M
import Data.Maybe

import Extended.Text (Text)
import Extended.Text qualified as T

import App.Types
import App.Result
import App.Internal hiding (decode)

import HKD.HKD

import Helpers.App
import Helpers.Database
import Helpers.Monad

import Test.Hspec
import Test.QuickCheck
import Data.String

propPostsEntity :: forall e. 
    ( ToJSON (e Create) 
    , FromJSON (e (Front Create))
    , ToJSON (e (Front Create))
    , FromRowOfT (ID (e Create))
    , ToRowOfT (e Create)
    ) => Text -> TestDB -> e Create -> Property
propPostsEntity path db e = property $ not (alreadyExists e db) ==> do
    x <- runTest 
        ( withBody (eCreateToFrontCreate e)
        . withPostPath path)
        ( withDB db )
    case x of
        (Left err, _) -> print err
        (Right (ResText t), st) -> do
            let Right aID = T.read @(ID (e Create)) t
                eFromDB = fromESum @(e Create)
                    $ fromJust $ M.lookup (toIDSum @(ID (e Create)) aID) (tDB st)
            M.lookup (toIDSum @(ID (e Create)) aID) (tDB st) `shouldBe` Just (toESum e)

eCreateToFrontCreate :: forall e.
    ( ToJSON (e Create)
    , FromJSON (e (Front Create))
    ) => e Create -> e (Front Create)
eCreateToFrontCreate ec = fromJust $ decode @(e (Front Create)) $ encode ec

propPostsNotIdempotent :: forall e. 
    ( ToJSON (e Create) 
    , FromJSON (e (Front Create))
    , ToJSON (e (Front Create))
    , FromRowOfT (ID (e Create))
    , ToRowOfT (e Create)
    ) => Text -> TestDB -> e Create -> Property
propPostsNotIdempotent path db e = property $ not (alreadyExists e db) ==> do
    st <- execTest 
        ( withBody (eCreateToFrontCreate e)
        . withPostPath path)
        ( withDB db)
    res <- evalTest 
        ( withBody (eCreateToFrontCreate e)
        . withPostPath path)
        ( withDB $ tDB st)
    res `shouldBe` Left (AlreadyExists "")

propPostsParsingFail :: forall e. 
    ( FromJSON (e (Front Create))
    ) => Text -> Value -> Property
propPostsParsingFail path (fromString . show -> obj) = 
    property $ isNothing (decode @(e (Front Create)) obj) ==> do
        Left res <- evalTest 
            ( withBLBody obj
            . withPostPath path )
            id
        res `shouldSatisfy ` isParsingError
  where
    isParsingError (ParsingErr _) = True
    isParsingError _ = False
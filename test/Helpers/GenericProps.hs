{-# LANGUAGE ViewPatterns #-}
module Helpers.GenericProps where

import Data.Aeson
import Data.Data
import Data.ByteString.Lazy qualified as BL 
import Data.IntMap qualified as IM
import Data.Maybe

import Entity.Internal
import Extended.Text (Text)
import Extended.Text qualified as T

import App.Types
import App.Result
import App.Internal hiding (decode)

import HKD.HKD

import Helpers.App
import Helpers.Entity
import Helpers.Database
import Helpers.Internal
import Helpers.Monad

import Test.Hspec
import Test.QuickCheck
import Data.String
import Data.Kind
import Data.List (sort)
import Control.Monad (replicateM)
import Unsafe.Coerce

-- type GPropsConstr e a =
--     ( TestEntity e 
--     , ToDisplay (e a) ~ e Display
--     , ToDisplay (e Display) ~ e Display
--     , Show (e a)
--     , Eq   (e a)
--     , Ord  (e a)
--     )


type GPropsConstr e a =
    ( TestEntity (e Display) 
    , TestEntity (e a) 
    , ToDisplay  (e a)       ~ e Display
    , ToDisplay  (e Display) ~ e Display
    , Typeable    e
    , Show       (e a)
    , Eq         (e a)
    , Ord        (e a)
    )

-- | Post

propPostsEntity :: forall e. 
    ( GPropsConstr e Create 
    , ToJSON      (e Create) 
    , FromJSON    (e (Front Create))
    , ToJSON      (e (Front Create))
    ) => Text -> TDB e -> e Create -> Property
propPostsEntity path db e = property $ not (alreadyExists e db) ==> do
    x <- runTest 
        ( withBody (toFrontCreate e)
        . withPostPath path)
        ( withDatabase @e db)
    case x of
        (Right (ResText t), st) -> do
            let Right (ID eID) = T.read @(ID (e Display)) t
            fromDisplay 
                <$> IM.lookup eID (extractTestDatabaseFromTestState @(e Create) st) 
                    `shouldBe` Just e
        (Left err, _) -> error $ show err

propPostsAlreadyExists :: forall e.
    ( GPropsConstr e Create 
    , FromJSON    (e (Front Create))
    , ToJSON (e (Front Create))
    ) => Text -> TDB e -> ID (e Display) -> e Display -> Property
propPostsAlreadyExists path db (ID eID) e = property $ do
    Left err <- evalTest 
        ( withBody (toFrontCreate e)
        . withPostPath path)
        ( withDatabase @e $ IM.insert eID e db)
    err `shouldSatisfy` isAlreadyExistsError

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

-- | Get

propGetEntities :: forall (e :: Type -> Type).
    ( GPropsConstr e (Front Display)
    , ToJSON (e (Front Display))
    ) => Text -> TDB e -> Property
propGetEntities path db = property $ condition ==> do
    res <- evalTest (withGetPath path) (withDatabase @e db)
    case res of
        Left err -> error $ show err
        Right (ResJSON j) -> do
            let es = map (\(eID,e) -> Entity (ID eID) $ fromDisplay e) $ IM.toList db    
            j `shouldBe` encode @[Entity e (Front Display)] es
  where
    condition = length (IM.toList db) < testPaginationConstant

newtype BigTDB e = BigTDB {unBigTDB :: TDB e}
deriving instance Show (e Display) => Show      (BigTDB e)
instance Arbitrary     (e Display) => Arbitrary (BigTDB e) where
    arbitrary = do
        n <- choose (1000,2000)
        BigTDB . mconcat <$> replicateM n arbitrary

propGetEntitiesIsPaginated :: forall (e :: Type -> Type).
    ( GPropsConstr e (Front Display)
    , ToJSON (e (Front Display))
    ) => Text -> BigTDB e -> Property
propGetEntitiesIsPaginated path (unBigTDB -> db) = property $ do
    res <- evalTest (withGetPath path) (withDatabase @e db)
    case res of
        Left err -> error $ show err
        Right (ResJSON j) -> let Just os = decode @(Maybe [Object]) j
            in length os <= testPaginationConstant `shouldBe` True

propGetEntitiesWithLimitOffset :: forall (e :: Type -> Type).
    ( GPropsConstr e (Front Display)
    , ToJSON (e (Front Display))
    ) => Text -> Int -> Int -> BigTDB e -> Property
propGetEntitiesWithLimitOffset path limit offset (unBigTDB -> db) = property $ do
    res <- evalTest 
        ( withGetPath path
        . withLimit limit
        . withOffset offset) 
        ( withDatabase @e db)
    case res of
        Left err -> error $ show err
        Right (ResJSON j) -> do
            j `shouldBe`  encode 
                (take (min limit testPaginationConstant)
                $ drop offset
                $ map (\(eID, e) -> Entity (ID eID) $ fromDisplay @(e (Front Display)) e) 
                $ IM.toList db)

propGetEntity :: forall (e :: Type -> Type).
    ( GPropsConstr e (Front Display)
    , ToJSON (e (Front Display))
    ) => Text -> TDB e -> ID (e Display) -> Property
propGetEntity path db (ID eID) = property $ eID `elem` IM.keys db ==> do
    res <- evalTest withPath (withDatabase @e db)
    case res of
        Left err -> error $ show err
        Right (ResJSON j) -> j `shouldBe` 
            encode ((\(Just e) -> Entity (ID eID) (fromDisplay @(e (Front Display)) e)) 
                $ IM.lookup eID db)
  where
    withPath = withGetPath $ path <> "/" <> T.show eID

propGetEntityDoesntExists :: forall (e :: Type -> Type).
    ( GPropsConstr e (Front Display)
    ) => Text -> TDB e -> ID (e Display) -> Property
propGetEntityDoesntExists path db (ID eID) = property $ eID `notElem` IM.keys db ==> do
    Left err <- evalTest withPath (withDatabase @e db)
    err `shouldSatisfy` isEntityNotFoundError
  where
    withPath = withGetPath $ path <> "/" <> T.show eID

-- | Put

propPutEntity :: forall e.
    ( GPropsConstr e (Front Update)
    , GPropsConstr e Display
    , ToJSON (e (Front Update))
    , TestUpdate (e (Front Update))
    ) => Text -> TDB e -> e (Front Update) -> ID (e Display) 
    -> Property
propPutEntity path db eu (ID eID) = property $ eID `IM.member` db ==> do
    st <- execTest 
        ( withBody eu
        . withPutPath (path <> "/" <> T.show eID))
        ( withDatabase @e db )
    let Just e = IM.lookup eID db
    IM.lookup eID (extractTestDatabaseFromTestState @(e (Front Update)) st) 
        `shouldBe` Just (eu >/ e) 

propPutEntityDoesntExists :: forall e.
    ( ToJSON (e (Front Update))
    , GPropsConstr e (Front Update)
    ) => Text -> TDB e -> e (Front Update) -> ID (e Display) 
    -> Property
propPutEntityDoesntExists path db eu (ID eID) = property $ not (eID `IM.member` db) ==> do
    Left err <- evalTest 
        ( withBody eu
        . withPutPath (path <> "/" <> T.show eID))
        ( withDatabase @e db )
    err `shouldSatisfy` isEntityNotFoundError 

propPutEntityParsingFail :: forall e. 
    ( FromJSON (e (Front Update))
    ) => Text -> TDB e -> Value -> ID (e Display) -> Property
propPutEntityParsingFail path db (fromString . show -> obj) (ID eID) = 
    property $ conditions ==> do
        Left res <- evalTest 
            ( withBLBody obj
            . withPutPath (path <> "/" <> T.show eID))
            id
        res `shouldSatisfy` isParsingError
  where
    conditions = isNothing (decode @(e (Front Update)) obj) &&
        eID `IM.member` db

-- | Delete

propDeleteEntity :: forall (e :: Type -> Type). 
    ( GPropsConstr e Display
    ) => Text -> TDB e -> ID (e Display) -> Property
propDeleteEntity path db (ID eID) = property $ eID `elem` IM.keys db ==> do
    st <- execTest 
        (withDeletePath $ path <> "/" <> T.show eID) 
        (withDatabase @e db)
    extractTestDatabaseFromTestState @(e Display) st `shouldBe` IM.delete eID db

propDeleteEntityDoesntExists :: forall (e :: Type -> Type). 
    ( GPropsConstr e Display
    ) => Text -> TDB e -> ID (e Display) -> Property
propDeleteEntityDoesntExists path db (ID eID) = property $ eID `notElem` IM.keys db ==> do
    Left res <- evalTest 
        (withDeletePath $ path <> "/" <> T.show eID) 
        (withDatabase @e db)
    res `shouldSatisfy` isEntityNotFoundError 



{-# LANGUAGE ViewPatterns #-}
module Helpers.GenericProps where

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
import Helpers.Entity
import Helpers.Database
import Helpers.Internal
import Helpers.Monad
import Helpers.Update

import Test.Hspec
import Test.QuickCheck
import Data.String
import Data.Kind
import Data.List (sort)

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
    , TestEntity e
    , DBOf (e Create) ~ M.Map (ID (e Display)) (e Display)
    ) => Text -> DBOf (e Create) -> e Create -> Property
propPostsEntity path db e = property $ not (alreadyExists e db) ==> do
    x <- runTest 
        ( withBody (eCreateToFrontCreate e)
        . withPostPath path)
        ( withDatabase @e db )
    case x of
        (Right (ResText t), st) -> do
            let Right eID = T.read @(ID (e Display)) t
            fromDisplay <$> M.lookup eID (dbFromTestState @e st) 
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

propGetEntities :: forall (e :: Type -> Type).
    ( TestEntity e
    , ToDisplay (e Display) ~ e Display
    , DBOf (e Display) ~ EMap (e Display)
    , FromJSON (e (Front Display))
    , Show (e (Front Display))
    , Eq (e (Front Display))
    , Ord (e (Front Display))
    ) => Text -> DBOf (e Display) -> Property
propGetEntities path db = property $ length (M.toList db) < testPaginationConstant ==> do
    Right (ResJSON es) <- evalTest (withGetPath path) (withDatabase @e db)
    let Right es' = eitherDecode es
    sort es' `shouldBe` sort (map eDisplayToFrontDisplay (M.elems db))

propGetEntitiesIsPaginated :: forall (e :: Type -> Type).
    ( TestEntity e
    , ToDisplay (e Display) ~ e Display
    , DBOf (e Display) ~ EMap (e Display)
    , FromJSON (e (Front Display))
    , Show (e (Front Display))
    , Eq (e (Front Display))
    , Ord (e (Front Display))
    ) => Text 
      -> DBOf (e Display) 
      -> DBOf (e Display) 
      -> DBOf (e Display) 
      -> DBOf (e Display) 
      -> DBOf (e Display) 
      -> Property
propGetEntitiesIsPaginated path db1 db2 db3 db4 db5 = 
    property $ do
        Right (ResJSON es) <- evalTest (withGetPath path) (withDatabase @e db)
        let Right es' = eitherDecode @[e (Front Display)] es
        length es' <= testPaginationConstant `shouldBe` True
  where
    db = mconcat [db1, db2, db3, db4, db5]

propGetEntitiesWithPage :: forall (e :: Type -> Type) a.
    ( TestEntity e
    , ToDisplay (e Display) ~ e Display
    , DBOf (e a) ~ EMap (e Display)
    , FromJSON (e (Front Display))
    , Show (e (Front Display))
    , Eq (e (Front Display))
    , Ord (e (Front Display))
    ) => Text -> Int -> DBOf (e a) -> DBOf (e a) -> DBOf (e a) ->  Property
propGetEntitiesWithPage path page db1 db2 db3 = let db = db1 <> db2 <> db3 in
    property $ page > 0 ==> do
        Right (ResJSON es) <- evalTest 
            ( withGetPath path
            . withPage page) 
            ( withDatabase @e db)
        let Right es' = eitherDecode @[e (Front Display)] es
        es' `shouldBe` take testPaginationConstant 
            ( drop (testPaginationConstant * (page - 1)) 
            $ map eDisplayToFrontDisplay $ M.elems db)

propGetEntity :: forall (e :: Type -> Type) a.
    ( TestEntity e
    , ToDisplay (e Display) ~ e Display
    , DBOf (e a) ~ EMap (e Display)
    , FromJSON (e (Front Display))
    , Show (e (Front Display))
    , Eq (e (Front Display))
    , Ord (e (Front Display))
    ) => Text -> DBOf (e a) -> ID (e Display) -> Property
propGetEntity path db eID = property $ eID `elem` M.keys db ==> do
    Right (ResJSON es) <- evalTest withPath (withDatabase @e db)
    let Right es' = eitherDecode es
    Just es' `shouldBe` eDisplayToFrontDisplay <$> M.lookup eID db
  where
    withPath = withGetPath $ path <> "/" <> T.show eID

propGetEntityDoesntExists :: forall (e :: Type -> Type) a.
    ( TestEntity e
    , ToDisplay (e Display) ~ e Display
    , DBOf (e a) ~ EMap (e Display)
    , FromJSON (e (Front Display))
    , Show (e (Front Display))
    , Eq (e (Front Display))
    , Ord (e (Front Display))
    ) => Text -> DBOf (e a) -> ID (e Display) -> Property
propGetEntityDoesntExists path db eID = property $ eID `notElem` M.keys db ==> do
    Left err <- evalTest withPath (withDatabase @e db)
    err `shouldSatisfy` isEntityNotFoundError
  where
    withPath = withGetPath $ path <> "/" <> T.show eID

propPutEntity :: forall e.
    ( TestUpdate e
    , ToJSON (e (Front Update))
    , TestEntity e
    , ToDisplay (e Display) ~ e Display
    , Show (e Display)
    , Eq (e Display)
    ) => Text -> EMap (e Display) -> e (Front Update) -> ID (e Display) 
    -> Property
propPutEntity path db eu eID = property $ eID `M.member` db ==> do
    st <- execTest 
        ( withBody eu
        . withPutPath (path <> "/" <> T.show eID))
        ( withDatabase @e db )
    let Just e = M.lookup eID db
    M.lookup eID (dbFromTestState @e st) `shouldBe` Just (eu >/ e) 

propPutEntityDoesntExists :: forall e.
    ( ToJSON (e (Front Update))
    , TestEntity e
    , ToDisplay (e Display) ~ e Display
    ) => Text -> EMap (e Display) -> e (Front Update) -> ID (e Display) 
    -> Property
propPutEntityDoesntExists path db eu eID = property $ not (eID `M.member` db) ==> do
    Left res <- evalTest 
        ( withBody eu
        . withPutPath (path <> "/" <> T.show eID))
        ( withDatabase @e db )
    res `shouldSatisfy` isEntityNotFoundError 

propPutParsingFail :: forall e. 
    ( FromJSON (e (Front Update))
    ) => Text -> EMap (e Display) -> Value -> ID (e Display) -> Property
propPutParsingFail path db (fromString . show -> obj) eID = 
    property $ conditions ==> do
        Left res <- evalTest 
            ( withBLBody obj
            . withPutPath (path <> "/" <> T.show eID))
            id
        res `shouldSatisfy` isParsingError
  where
    conditions = isNothing (decode @(e (Front Update)) obj) &&
            eID `M.member` db

propDeleteEntity :: forall (e :: Type -> Type). 
    ( TestEntity e
    , DBOf (e Delete) ~ M.Map (ID (e Display)) (e Display)
    , ToDisplay (e Display) ~ e Display
    , Show (e Display)
    , Eq (e Display)
    ) => Text -> DBOf (e Display) -> ID (e Display) -> Property
propDeleteEntity path db eID = property $ eID `elem` M.keys db ==> do
    st <- execTest 
        (withDeletePath $ path <> "/" <> T.show eID) 
        (withDatabase @e db)
    dbFromTestState @e st `shouldBe` M.delete eID db

propDeleteEntityDoesntExists :: forall (e :: Type -> Type). 
    ( TestEntity e
    , DBOf (e Delete) ~ M.Map (ID (e Display)) (e Display)
    , ToDisplay (e Display) ~ e Display
    , Show (e Display)
    , Eq (e Display)
    ) => Text -> DBOf (e Display) -> ID (e Display) -> Property
propDeleteEntityDoesntExists path db eID = property $ eID `notElem` M.keys db ==> do
    Left res <- evalTest 
        (withDeletePath $ path <> "/" <> T.show eID) 
        (withDatabase @e db)
    res `shouldSatisfy` isEntityNotFoundError 
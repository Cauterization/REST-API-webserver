module Helpers.Internal where

import App.Types
import App.Internal
import Data.Aeson

import Entity.Internal
import Extended.Text (Text)
import Extended.Text qualified as T

import HKD.HKD

import Test.QuickCheck
import Control.Monad (replicateM)

testPaginationConstant :: PaginationSize
testPaginationConstant = 20

isParsingError :: AppError -> Bool
isParsingError ParsingErr{} = True
isParsingError _            = False

isEntityNotFoundError :: AppError -> Bool
isEntityNotFoundError EntityNotFound{} = True
isEntityNotFoundError _                = False

isAlreadyExistsError :: AppError -> Bool
isAlreadyExistsError AlreadyExists{} = True
isAlreadyExistsError _               = False

isUnathorizedError :: AppError -> Bool
isUnathorizedError Unathorized{} = True
isUnathorizedError_              = False

deriving instance (Eq (e a),  Eq  (NamedID "_id" a e)) => Eq  (Entity e a)
deriving instance (Ord (e a), Ord (NamedID "_id" a e)) => Ord (Entity e a)

instance Arbitrary (ID e) where
    arbitrary = ID <$> chooseInt (1, 20)

instance Arbitrary Text where
    arbitrary = T.pack <$> arbitrary

instance Arbitrary Date where
    arbitrary = toEnum <$> arbitrary

instance {-# OVERLAPPING #-} ToJSON (Maybe NotAllowedFromFront) where
    toJSON _ = Null

instance Arbitrary Value where
    arbitrary = chooseInt (3, 6) >>= \case
        -- 1 -> Object <$> arbitrary
        -- 2 -> Array . fromList <$> do
        --     len <- choose (1, 10)
        --     replicateM len arbitrary
        3 -> String <$> arbitrary
        4 -> Number . realToFrac <$> arbitrary @Double
        5 -> Bool   <$> arbitrary 
        6 -> pure Null 

deriving instance Show (e Display) => Show (Entity e Display)

instance Arbitrary (e Display) => Arbitrary (Entity e Display) where
    arbitrary = do
        entityID <- arbitrary
        entity   <- arbitrary
        pure Entity{..}

deriving instance Eq       NotDisplayed
deriving instance Ord      NotDisplayed
instance {-# OVERLAPPING #-} FromJSON (Maybe NotDisplayed) where
    parseJSON _ = pure Nothing

deriving instance Show     NotUpdated
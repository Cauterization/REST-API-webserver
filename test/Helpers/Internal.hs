module Helpers.Internal where

import App.Types
import App.Internal
import App.Config
import Data.Aeson
import Data.Time qualified as Time
import Data.Char
import Data.ByteString.Lazy qualified as LB
import Database.Database qualified as Database

import Entity.Internal
import Extended.Text (Text)
import Extended.Text qualified as T

import HKD.HKD

import Test.QuickCheck
import Control.Monad (replicateM)

testConfig :: Config
testConfig = 
    let dbConfig = Database.Config{cPagSize = testPaginationConstant}
    in Config{cDatabase = dbConfig}

testPaginationConstant :: PaginationSize
testPaginationConstant = 20

testDate :: Date
testDate = Time.fromGregorian 1 2 3

testToken :: Token
testToken = "super unique token"

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
isUnathorizedError _             = False

isWrongPasswordError :: AppError -> Bool
isWrongPasswordError WrongPassword{} = True
isWrongPasswordError _               = False

isRequestHeadersError :: AppError -> Bool
isRequestHeadersError RequestHeadersErr{} = True
isRequestHeadersError _                   = False


deriving instance Ord (e a) => Ord (Entity e a)

instance Arbitrary (ID e) where
    arbitrary = ID <$> chooseInt (1, 20)

instance Arbitrary Text where
    arbitrary = T.pack <$> arbitrary

instance Arbitrary LB.ByteString where
    arbitrary = LB.pack <$> arbitrary

instance Arbitrary Date where
    arbitrary = toEnum <$> arbitrary

instance {-# OVERLAPPING #-} ToJSON (Maybe NotAllowedFromFront) where
    toJSON _ = Null

instance Arbitrary Value where
    arbitrary = chooseInt (3, 6) >>= \case
        3 -> String <$> arbitrary
        4 -> Number . realToFrac <$> arbitrary @Double
        5 -> Bool   <$> arbitrary 
        6 -> pure Null 

instance Arbitrary (e Display) => Arbitrary (Entity e Display) where
    arbitrary = do
        entityID <- arbitrary
        entity   <- arbitrary
        pure Entity{..}

deriving instance Ord      NotDisplayed
instance {-# OVERLAPPING #-} FromJSON (Maybe NotDisplayed) where
    parseJSON _ = pure Nothing

deriving instance Show       NotUpdated
instance {-# OVERLAPPING #-} ToJSON   (Maybe NotUpdated) where
    toJSON _ = Null

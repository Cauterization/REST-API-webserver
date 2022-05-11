module Helpers.Internal where

import App.Config ( Config(Config, cDatabase, cAddress, cPort) )
import App.Internal
    ( AppError(RequestHeadersErr, ParsingErr, EntityNotFound,
               AlreadyExists, Unathorized, WrongPassword) )
import App.Types ( ID(ID), Token, Date, PaginationSize )
import Control.Monad (replicateM)
import Data.Aeson
    ( Value(Null, String, Number, Bool),
      FromJSON(parseJSON),
      ToJSON(toJSON) )
import Data.ByteString.Lazy qualified as LB
import Data.Time qualified as Time
import Database.Database qualified as Database
import Entity.Internal ( Entity(..) )
import Extended.Text (Text)
import Extended.Text qualified as T
import HKD.HKD
    ( Display, NotAllowedFromFront, NotDisplayed, NotUpdated )
import Test.QuickCheck ( chooseInt, Arbitrary(arbitrary) )

testConfig :: Config
testConfig =
  Config
    { cDatabase =
        Database.Config
          { cPagSize = testPaginationConstant
          },
      cAddress = testAddressConstant,
      cPort = testPortConstant
    }

testPaginationConstant :: PaginationSize
testPaginationConstant = 20

testAddressConstant :: Text
testAddressConstant = "TESTURAPI.COM"

testPortConstant :: Int
testPortConstant = 3000

testDate :: Date
testDate = Time.fromGregorian 1 2 3

testToken :: Token
testToken = "super unique token"

isParsingError :: AppError -> Bool
isParsingError ParsingErr {} = True
isParsingError _ = False

isEntityNotFoundError :: AppError -> Bool
isEntityNotFoundError EntityNotFound {} = True
isEntityNotFoundError _ = False

isAlreadyExistsError :: AppError -> Bool
isAlreadyExistsError AlreadyExists {} = True
isAlreadyExistsError _ = False

isUnathorizedError :: AppError -> Bool
isUnathorizedError Unathorized {} = True
isUnathorizedError _ = False

isWrongPasswordError :: AppError -> Bool
isWrongPasswordError WrongPassword {} = True
isWrongPasswordError _ = False

isRequestHeadersError :: AppError -> Bool
isRequestHeadersError RequestHeadersErr {} = True
isRequestHeadersError _ = False

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
  arbitrary =
    chooseInt (3, 6) >>= \case
      3 -> String <$> arbitrary
      4 -> Number . realToFrac <$> arbitrary @Double
      5 -> Bool <$> arbitrary
      6 -> pure Null

instance Arbitrary (e Display) => Arbitrary (Entity e Display) where
  arbitrary = do
    entityID <- arbitrary
    entity <- arbitrary
    pure Entity {..}

deriving instance Ord NotDisplayed

instance {-# OVERLAPPING #-} FromJSON (Maybe NotDisplayed) where
  parseJSON _ = pure Nothing

deriving instance Show NotUpdated

instance {-# OVERLAPPING #-} ToJSON (Maybe NotUpdated) where
  toJSON _ = Null

instance {-# OVERLAPPING #-} Arbitrary (Maybe NotAllowedFromFront) where
  arbitrary = pure Nothing

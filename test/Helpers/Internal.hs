module Helpers.Internal where

import App.Config (Config (Config, cAddress, cDatabase, cPort))
import App.Internal
  ( AppError (..),
  )
import App.Types (Date, ID (ID), PaginationSize, Token)
import Control.Monad (replicateM)
import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    Value (Bool, Null, Number, String),
  )
import Data.ByteString.Lazy qualified as LB
import Data.Time qualified as Time
import Database.Database qualified as Database
import Entity.Internal (Entity (..))
import Extended.Text (Text)
import Extended.Text qualified as T
import HKD.HKD
  ( Display,
    NotAllowedFromFront,
    NotDisplayed,
    NotUpdated,
  )
import Test.QuickCheck (Arbitrary (arbitrary), chooseInt)

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

isParsingError,
  isEntityNotFoundError,
  isAlreadyExistsError,
  isUnathorizedError,
  isWrongPasswordError,
  isRequestHeadersError,
  isAmbiguousPatternsError,
  isAdminAccessViolationError ::
    AppError -> Bool
isParsingError ParsingError {} = True
isParsingError _ = False
isEntityNotFoundError EntityNotFound {} = True
isEntityNotFoundError _ = False
isAlreadyExistsError AlreadyExists {} = True
isAlreadyExistsError _ = False
isUnathorizedError Unathorized {} = True
isUnathorizedError _ = False
isWrongPasswordError WrongPassword {} = True
isWrongPasswordError _ = False
isRequestHeadersError RequestHeadersError {} = True
isRequestHeadersError _ = False
isAmbiguousPatternsError RouterAmbiguousPatterns {} = True
isAmbiguousPatternsError _ = False
isAdminAccessViolationError AdminAccessViolation {} = True
isAdminAccessViolationError _ = False

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

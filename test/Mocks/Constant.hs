{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Mocks.Constant where

import App.Types
import Data.Time qualified as Time
import Extended.Text

testPaginationConstant :: PaginationSize
testPaginationConstant = 20

testAddressConstant :: Text
testAddressConstant = "http://localhost"

testPortConstant :: Int
testPortConstant = 3000

testDateConstant :: Date
testDateConstant = Time.fromGregorian 1 2 3

testTokenConstant :: Token
testTokenConstant = "super unique token"

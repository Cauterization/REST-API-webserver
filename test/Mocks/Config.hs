{-# LANGUAGE ImportQualifiedPost #-}

module Mocks.Config where

import App.Config
  ( Config (Config, cAddress, cDatabase, cLogger, cPort),
  )
import Database.Config qualified as Database
import Logger qualified
import Mocks.Constant
  ( testAddressConstant,
    testPaginationConstant,
    testPortConstant,
  )

testConfig :: Config
testConfig =
  Config
    { cDatabase =
        Database.Config
          { cPagSize = testPaginationConstant,
            cHost = "",
            cPort = 0,
            cUser = "",
            cPassword = ""
          },
      cAddress = testAddressConstant,
      cPort = testPortConstant,
      cLogger =
        Logger.Config
          { cVerbosity = Logger.Warning,
            cMode = Logger.Display,
            cFilePath = ""
          }
    }

module Mocks.Config where

import App.Config
  ( Config (..),
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
    { cDB =
        Database.Config
          { cPagSize = testPaginationConstant,
            cHost = "",
            cPort = 0,
            cUser = "",
            cPassword = "",
            cDatabase = ""
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

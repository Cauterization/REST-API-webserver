{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Mocks.Config where

import App.Config
import Database.Config qualified as Database
import Logger qualified
import Mocks.Constant

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

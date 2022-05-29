{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Mocks.With where

import App.AppT
import App.Path
import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Map qualified as M
import Extended.Text (Text)
import Extended.Text qualified as T
import Mocks.Run
import Mocks.TestMonad

withPath :: Method -> Text -> EnvEndo
withPath m p Env {..} = Env {envPath = m (T.splitOn "/" p), ..}

withPostPath, withGetPath, withDeletePath, withPutPath, withPublishPath :: Text -> EnvEndo
withPostPath = withPath POST
withGetPath = withPath GET
withPutPath = withPath PUT
withDeletePath = withPath DELETE
withPublishPath = withPath PUBLISH

withBody :: ToJSON a => a -> EnvEndo
withBody b e = e {envBody = encode b}

withBLBody :: BL.ByteString -> EnvEndo
withBLBody b e = e {envBody = b}

withFailedDelete :: StateEndo
withFailedDelete TestState {..} = TestState {deleteResult = pure 0, ..}

withFailedPut :: StateEndo
withFailedPut TestState {..} = TestState {putResult = pure 0, ..}

withLimit :: Int -> EnvEndo
withLimit l Env {..} = Env {envQParams = M.insert "limit" [T.show l] envQParams, ..}

withOffset :: Int -> EnvEndo
withOffset o Env {..} = Env {envQParams = M.insert "offset" [T.show o] envQParams, ..}

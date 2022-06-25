module Mocks.With where

import App.AppT
  ( Env (..),
  )
import App.Path (Method, Path (DELETE, GET, POST, PUBLISH, PUT))
import App.Types (ContentType, Token)
import Control.Monad.Writer (tell)
import Data.Aeson (ToJSON, encode)
import Data.ByteString.Lazy qualified as BL
import Data.Map qualified as M
import Extended.Text (Text)
import Extended.Text qualified as T
import Mocks.Run (EnvEndo)
import Mocks.TestMonad
  ( StateEndo,
    TestState (deleteResult, putResult),
  )

withPath :: Method -> Text -> EnvEndo
withPath m p e = e {envPath = m (T.splitOn "/" p)}

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
withFailedDelete s = s {deleteResult = pure 0}

withFailedPut :: StateEndo
withFailedPut s = s {putResult = pure 0}

withParam :: Show a => Text -> a -> EnvEndo
withParam p a Env {..} = Env {envQParams = M.insert p [T.show a] envQParams, ..}

withMaybeParam :: Show a => Text -> Maybe a -> EnvEndo
withMaybeParam p a e = maybe e (\x -> withParam p x e) a

withMaybeTextParam :: Text -> Maybe Text -> EnvEndo
withMaybeTextParam p a Env {..} =
  maybe Env {..} (\x -> Env {envQParams = M.insert p [x] envQParams, ..}) a

withLimit :: Int -> EnvEndo
withLimit = withParam "limit"

withOffset :: Int -> EnvEndo
withOffset = withParam "offset"

withToken :: Token -> EnvEndo
withToken t e = e {envToken = Just t}

woLogger :: EnvEndo
woLogger e = e {envLogger = \_ _ -> pure ()}

wLogger :: EnvEndo
wLogger e = e {envLogger = \v t -> tell [(v, t)]}

withContentType :: ContentType -> EnvEndo
withContentType ct e = e {envContentType = Just ct}

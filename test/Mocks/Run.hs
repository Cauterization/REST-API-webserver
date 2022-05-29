{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Mocks.Run where

import App.AppT
import App.Endpoints
import App.Error
import App.Path
import App.Result
import App.Router
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Data.Map qualified as M
import Extended.Text (Text)
import Extended.Text qualified as T
import Logger qualified
import Mocks.Config
import Mocks.Endpoints
import Mocks.TestMonad

type TestEnv = Env TestMonad

type EnvEndo = TestEnv -> TestEnv

defaultEnv :: TestEnv
defaultEnv =
  Env
    { envLogger = \v t -> when (v >= Logger.Warning) $ tell [(v, t)],
      envConn = (),
      envConfig = testConfig,
      envPath = POST [],
      envBody = "",
      envQParams = M.empty,
      envContentType = Nothing,
      envToken = Nothing
    }

runTest :: EnvEndo -> StateEndo -> IO (Either AppError AppResult, TestState)
runTest eEndo sEndo = do
  let ((res, log), st) = runTestMonad eEndo sEndo runState
  forM_ log (putStrLn . (\(v, t) -> show v <> ": " <> T.unpack t))
  pure (join res, st)

evalTest :: EnvEndo -> StateEndo -> IO (Either AppError AppResult)
evalTest = (fmap fst .) . runTest

execTest :: EnvEndo -> StateEndo -> IO TestState
execTest = (fmap snd .) . runTest

runTestMonad ::
  EnvEndo ->
  StateEndo ->
  ( State TestState (Either AppError (Either AppError AppResult), [(Logger.Verbosity, Text)]) ->
    TestState ->
    x
  ) ->
  x
runTestMonad eEndo sEndo f =
  flip f (sEndo initialState) $
    runWriterT $ runExceptT $ unTestM $ runRouterTest eEndo

runRouterTest :: EnvEndo -> TestMonad (Either AppError AppResult)
runRouterTest f =
  let Env {..} = f defaultEnv
   in runRouter @Main
        envLogger
        envConn
        envPath
        envBody
        envContentType
        envQParams
        envToken
        envConfig

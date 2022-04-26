module Helpers.App where

import Api.Author   qualified as Author
import Api.Category qualified as Category
import Api.User     qualified as User
import Api.Post
import Api.Put
import Api.Delete
import App.App
import App.Internal
import App.Result
import App.Router
import App.Types




import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Extra (whenM)
import Control.Monad.Reader
import Control.Monad.State hiding (get, put)
import Control.Monad.Writer 

import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Map qualified as M
import Database.Database

import Entity.Internal
import Entity.Author
import Entity.Category
import Entity.Tag
import Entity.User

import Extended.Text (Text)
import Extended.Text qualified as T

import Logger qualified

import Helpers.Database
import Helpers.Monad
import Helpers.Author
import Helpers.AuthorDB
import Helpers.CategoryDB
import Helpers.TagDB
import Helpers.User
import Helpers.UserDB
import Helpers.Internal

import Test.Hspec
import Test.QuickCheck
import Api.Get
import Api.ProtectedResources

runTest :: EnvMod -> StateMod -> IO (Either AppError AppResult, TestState)
runTest eMod sMod = do
    let ((res, log), st) = runTestMonad eMod sMod runState
    forM_ log (putStrLn . (\(v, t) -> show v <> ": " <> T.unpack t)) 
    pure (res, st)

evalTest :: EnvMod -> StateMod -> IO (Either AppError AppResult)
evalTest = (fmap fst . ) . runTest
    
execTest :: EnvMod -> StateMod ->  IO TestState
execTest = (fmap snd . ) . runTest

runTestMonad :: 
    EnvMod 
    -> StateMod
    ->  (  State TestState (Either AppError AppResult, [(Logger.Verbosity, Text)]) 
        -> TestState 
        -> x)
    -> x
runTestMonad eMod sMod f = flip f (sMod initialState)
    $ runWriterT $ runExceptT $ unTestM $ runRouterTest eMod

runRouterTest :: EnvMod -> TestMonad AppResult
runRouterTest f = let Env{..} = f defaultEnv in either throwError pure =<< 
    runRouterWith @Main
        envLogger 
        envConn 
        envPath 
        envBody 
        envQParams 
        envToken 
        envPagination 
        (pure ())

instance Routed Main TestDB where
    router = do
        -- addMiddleware protectedResources
        newRouter @User 
        newRouter @Author 
        newRouter @Tag 
        newRouter @Category

-- | Note that urls listed here doesn't have an "admin" prefix
--   coz we have separated tests for protected content

instance Routed User TestDB where
    router = do
        post    "users"              User.postUser
        get     "users/me"           User.getCurrentUser
        delete_ "users/{ID}" 
        post    "auth"               User.authUser

instance Routed Author TestDB where
    router = do
        post_   "authors"            
        get_    "authors"          
        get_    "authors/{ID}" 
        put_    "authors/{ID}"     
        delete_ "authors/{ID}"  

instance Routed Tag TestDB where
    router = do
        post_   "tags"       
        get_    "tags"    
        get_    "tags/{ID}"    
        put_    "tags/{ID}"
        delete_ "tags/{ID}"

instance Routed Category TestDB where
    router = do
        post_   "categories"
        -- get_    "categories"
        -- get_    "categories{ID}"
        -- put     "categories/{ID}" Category.putCategory     
        delete_ "categories/{ID}"       

defaultEnv :: Env TestMonad
defaultEnv  = Env 
    { envLogger     = \v t -> when (v >= Logger.Warning) $ tell [(v, t)]
    , envConn       = ()
    , envPagination = testPaginationConstant
    , envPath       = error "envPath"
    , envBody       = ""
    , envQParams    = M.empty
    , envToken      = Nothing
    } 

type EnvMod = Env TestMonad -> Env TestMonad

withBody :: ToJSON e => e -> EnvMod
withBody e Env{..} = Env{envBody = encode e, ..}

withBLBody :: BL.ByteString -> EnvMod
withBLBody bl Env{..} = Env{envBody = bl, ..}

withLimit :: Int -> EnvMod
withLimit l Env{..} = Env{envQParams = M.insert "limit" [T.show l] envQParams, ..}

withOffset :: Int -> EnvMod
withOffset o Env{..} = Env{envQParams = M.insert "offset" [T.show o] envQParams, ..}

withPath :: Method -> Text -> EnvMod
withPath m p Env{..} = Env{envPath = m (T.splitOn "/" p), ..}

withPostPath, withGetPath, withDeletePath, withPutPath, withPublishPath :: Text -> EnvMod
withPostPath    = withPath POST
withGetPath     = withPath GET
withPutPath     = withPath PUT
withDeletePath  = withPath DELETE
withPublishPath = withPath PUBLISH

withToken :: Token -> EnvMod
withToken t Env{..} = Env{envToken = Just t, ..}

woLogger :: EnvMod
woLogger Env{..} = Env{envLogger = const . const $ pure (), ..}

instance HasDatabase (AppT TestMonad) where

    type Database (AppT TestMonad) = TestDB 

    liftDatabase = handle (throwM . fromDBException) . lift

    getDatabaseConnection = pure ()

    getPaginationSize = asks envPagination



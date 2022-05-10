{-# LANGUAGE ViewPatterns #-}

module Helpers.App where

import Api.Author   qualified as Author
import Api.Article  qualified as Article
import Api.Category qualified as Category
import Api.User     qualified as User
import Api.Picture  qualified as Picture
import Api.Draft    qualified as Draft
import Api.Post
import Api.Put
import Api.Delete
import Api.Get
import Api.Publish
import Api.ProtectedResources
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
import Data.Char
import Data.Map qualified as M
import Database.Database

import Entity.Internal
import Entity.Author
import Entity.Draft
import Entity.Article
import Entity.Category
import Entity.Tag
import Entity.User
import Entity.Picture

import Extended.Text (Text)
import Extended.Text qualified as T

import Logger qualified

import Type.Reflection qualified as Refl

import Helpers.Database
import Helpers.Monad
import Helpers.Author
import Helpers.AuthorDB
import Helpers.Article
import Helpers.ArticleDB
import Helpers.Draft
import Helpers.DraftDB
import Helpers.CategoryDB
import Helpers.TagDB
import Helpers.User
import Helpers.UserDB
import Helpers.PictureDB
import Helpers.Internal

import Test.Hspec
import Test.QuickCheck


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

runTestMonadNoMods :: AppT TestMonad x -> Either AppError x
runTestMonadNoMods = fst 
                   . flip evalState initialState 
                   . runWriterT 
                   . runExceptT 
                   . unTestM 
                   . flip runReaderT defaultEnv 
                   . unApp

runRouterTest :: EnvMod -> TestMonad AppResult
runRouterTest f = let Env{..} = f defaultEnv in either throwError pure =<< 
    runRouterWith @Main
        envLogger 
        envConn 
        envPath 
        envBody
        envContentType 
        envQParams 
        envToken 
        envConfig
        (pure ())

instance Routed Main (AppT TestMonad) where
    router = do
        -- addMiddleware protectedResources
        newRouter @User 
        newRouter @Author 
        newRouter @Tag 
        newRouter @Category
        newRouter @Article
        newRouter @Picture
        newRouter @Draft

-- | Note that urls listed here doesn't have an "admin" prefix
--   coz we have separated tests for protected content

instance Routed User (AppT TestMonad) where
    router = do
        post    "users"              User.postUser
        get     "users/me"           User.getMe
        delete_ "users/{ID}" 
        post    "auth"               User.authUser

instance Routed Author (AppT TestMonad) where
    router = do
        post_   "authors"            
        get_    "authors"          
        get_    "authors/{ID}" 
        put_    "authors/{ID}"     
        delete_ "authors/{ID}"  

instance Routed Tag (AppT TestMonad) where
    router = do
        post_   "tags"       
        get_    "tags"    
        get_    "tags/{ID}"    
        put_    "tags/{ID}"
        delete_ "tags/{ID}"

instance Routed Category (AppT TestMonad) where
    router = do
        post_   "categories"
        -- get_    "categories"
        -- get_    "categories{ID}"
        -- put     "categories/{ID}" Category.putCategory     
        delete_ "categories/{ID}"       

instance Routed Article (AppT TestMonad) where
    router = do
        get      "articles"                  Article.getArticles
        get_     "articles/{ID}"    

instance Routed Draft (AppT TestMonad) where
    router = do
        addMiddleware                        Draft.draftAccess
        post     "drafts"                    Draft.postDraft
        get      "drafts"                    Draft.getDrafts
        get_     "drafts/{ID}"              
        put_     "drafts/{ID}"              
        delete_  "drafts/{ID}"
        publish_ "drafts/{ID}"

instance Routed Picture (AppT TestMonad) where
    router = do
        post     "pictures"                  Picture.postPicture
        get      "pictures/{ID}"             Picture.getPicture
        delete_  "pictures/{ID}"

defaultEnv :: Env TestMonad
defaultEnv  = Env 
    { envLogger      = \v t -> when (v >= Logger.Warning) $ tell [(v, t)]
    , envConn        = ()
    , envConfig      = testConfig
    , envPath        = error "envPath"
    , envBody        = ""
    , envQParams     = M.empty
    , envContentType = error "envContentType"
    , envToken       = Nothing
    } 

type EnvMod = Env TestMonad -> Env TestMonad

withBody :: ToJSON e => e -> EnvMod
withBody e Env{..} = Env{envBody = encode e, ..}

withBLBody :: BL.ByteString -> EnvMod
withBLBody bl Env{..} = Env{envBody = bl, ..}

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

withContentType :: ContentType -> EnvMod
withContentType ct Env{..} = Env{envContentType = Just ct, ..}

withLimit :: Int -> EnvMod
withLimit l Env{..} = Env{envQParams = M.insert "limit" [T.show l] envQParams, ..}

withOffset :: Int -> EnvMod
withOffset o Env{..} = Env{envQParams = M.insert "offset" [T.show o] envQParams, ..}

withParam :: forall p. (Show p, Refl.Typeable p) => Text -> p -> EnvMod
withParam (T.map toLower -> pp) pv Env{..}
    | Just Refl.HRefl <- Refl.typeRep @p `Refl.eqTypeRep` Refl.typeRep @Text 
        = Env{envQParams = M.insert pp [pv] envQParams, .. }
    | otherwise 
        = Env{envQParams = M.insert pp [T.show pv] envQParams, .. }

withParam' :: Text -> Text -> EnvMod
withParam' (T.map toLower -> pp) pv Env{..} = Env{envQParams = M.insert pp [pv] envQParams, .. }

-- withDateParam :: Text -> Date -> EnvMod
-- withDateParam pp pv Env{..} = Env{envQParams = M.insert pp [T.show pv] envQParams, .. }

-- withNumParam :: Text -> Int -> EnvMod
-- withNumParam pp pv Env{..} = Env{envQParams = M.insert pp [T.show pv] envQParams, .. }

instance HasDatabase (AppT TestMonad) where

    type Database (AppT TestMonad) = TestDB 

    liftDatabase = handle (throwM . fromDBException) . lift

    getDatabaseConnection = pure ()



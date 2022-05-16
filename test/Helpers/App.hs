module Helpers.App where

import Api.Article qualified as Article
import Api.Author qualified as Author
import Api.Category qualified as Category
import Api.Delete ( delete_ )
import Api.Draft qualified as Draft
import Api.Get ( get_ )
import Api.Picture qualified as Picture
import Api.Post ( post_ )
import Api.Publish ( publish_ )
import Api.Put ( put_ )
import Api.User qualified as User
import App.App ( Main )
import App.Internal
    ( fromDBException, AppError, AppT(unApp), Env(..) )
import App.Result ( AppResult, text )
import App.Router
    ( Routed(..), addMiddleware, get, newRouter, post, runRouter )
import App.Types
    ( ContentType,
      Method,
      Path(PUBLISH, POST, GET, PUT, DELETE),
      Token )
import Control.Monad.Catch ( MonadThrow(throwM), handle )
import Control.Monad.Except
    ( when,
      MonadTrans(lift),
      forM_,
      runExceptT,
      MonadError(throwError) )
import Control.Monad.Extra (whenM)
import Control.Monad.Reader ( ReaderT(runReaderT) )
import Control.Monad.State ( evalState, runState, State )
import Control.Monad.Writer
    ( WriterT(runWriterT), MonadWriter(tell) )
import Data.Aeson ( ToJSON, encode )
import Data.ByteString.Lazy qualified as BL
import Data.Char ( toLower )
import Data.Kind ( Type )
import Data.Map qualified as M
import Database.Database ( HasDatabase(..) )
import Entity.Article ( Article )
import Entity.Author ( Author )
import Entity.Category ( Category )
import Entity.Draft ( Draft )
import Entity.Picture ( Picture )
import Entity.Tag ( Tag )
import Entity.User ( User )
import Extended.Text ( Text )
import Extended.Text qualified as T
import Helpers.ArticleDB ()
import Helpers.Database ( TestDB, StateMod )
import Helpers.DraftDB ()
import Helpers.Internal ( testConfig )
import Helpers.Monad
    ( TestState, TestMonad(unTestM), initialState )
import Helpers.PictureDB ()
import Logger qualified
import Type.Reflection qualified as Refl
import Api.ProtectedResources (protectedResources)

runTest :: EnvMod -> StateMod -> IO (Either AppError AppResult, TestState)
runTest eMod sMod = do
  let ((res, log), st) = runTestMonad eMod sMod runState
  forM_ log (putStrLn . (\(v, t) -> show v <> ": " <> T.unpack t))
  pure (res, st)

evalTest :: EnvMod -> StateMod -> IO (Either AppError AppResult)
evalTest = (fmap fst .) . runTest

execTest :: EnvMod -> StateMod -> IO TestState
execTest = (fmap snd .) . runTest

runTestMonad ::
  EnvMod ->
  StateMod ->
  ( State TestState (Either AppError AppResult, [(Logger.Verbosity, Text)]) ->
    TestState ->
    x
  ) ->
  x
runTestMonad eMod sMod f =
  flip f (sMod initialState) $
    runWriterT $ runExceptT $ unTestM $ runRouterTest eMod

runTestMonadNoMods :: AppT TestMonad x -> Either AppError x
runTestMonadNoMods =
  fst
    . flip evalState initialState
    . runWriterT
    . runExceptT
    . unTestM
    . flip runReaderT defaultEnv
    . unApp

runRouterTest :: EnvMod -> TestMonad AppResult
runRouterTest f =
  let Env {..} = f defaultEnv
   in either throwError pure
        =<< runRouter @Main
          envLogger
          envConn
          envPath
          envBody
          envContentType
          envQParams
          envToken
          envConfig

instance Routed Main (AppT TestMonad) where
  router = do
    newRouter @User
    newRouter @Author
    newRouter @Tag
    newRouter @Category
    newRouter @Article
    newRouter @Picture
    newRouter @Draft
    newRouter @OtherTests

instance Routed User (AppT TestMonad) where
  router = do
    post "users" User.postUser
    get "users/me" User.getMe
    delete_ "users/{ID}"
    post "auth" User.authUser

instance Routed Author (AppT TestMonad) where
  router = do
    post_ "authors"
    get_ "authors"
    get_ "authors/{ID}"
    put_ "authors/{ID}"
    delete_ "authors/{ID}"

instance Routed Tag (AppT TestMonad) where
  router = do
    post_ "tags"
    get_ "tags"
    get_ "tags/{ID}"
    put_ "tags/{ID}"
    delete_ "tags/{ID}"

instance Routed Category (AppT TestMonad) where
  router = do
    post_ "categories"
    delete_ "categories/{ID}"

instance Routed Article (AppT TestMonad) where
  router = do
    get "articles" Article.getArticles
    get_ "articles/{ID}"

instance Routed Draft (AppT TestMonad) where
  router = do
    addMiddleware Draft.draftAccess
    post "drafts" Draft.postDraft
    get "drafts" Draft.getDrafts
    get_ "drafts/{ID}"
    put_ "drafts/{ID}"
    delete_ "drafts/{ID}"
    publish_ "drafts/{ID}"

instance Routed Picture (AppT TestMonad) where
  router = do
    post "pictures" Picture.postPicture
    get "pictures/{ID}" Picture.getPicture
    delete_ "pictures/{ID}"

-- | Tests for router

data OtherTests :: Type -> Type

instance Routed OtherTests (AppT TestMonad) where
  router = do
    post "ambiguous" $ pure $ text ""
    post "ambiguous" $ pure $ text ""
   


defaultEnv :: Env TestMonad
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

type EnvMod = Env TestMonad -> Env TestMonad

withBody :: ToJSON e => e -> EnvMod
withBody e Env {..} = Env {envBody = encode e, ..}

withBLBody :: BL.ByteString -> EnvMod
withBLBody bl Env {..} = Env {envBody = bl, ..}

withPath :: Method -> Text -> EnvMod
withPath m p Env {..} = Env {envPath = m (T.splitOn "/" p), ..}

withPostPath, withGetPath, withDeletePath, withPutPath, withPublishPath :: Text -> EnvMod
withPostPath = withPath POST
withGetPath = withPath GET
withPutPath = withPath PUT
withDeletePath = withPath DELETE
withPublishPath = withPath PUBLISH

withToken :: Token -> EnvMod
withToken t Env {..} = Env {envToken = Just t, ..}

woLogger :: EnvMod
woLogger Env {..} = Env {envLogger = const . const $ pure (), ..}

withContentType :: ContentType -> EnvMod
withContentType ct Env {..} = Env {envContentType = Just ct, ..}

withLimit :: Int -> EnvMod
withLimit l Env {..} = Env {envQParams = M.insert "limit" [T.show l] envQParams, ..}

withOffset :: Int -> EnvMod
withOffset o Env {..} = Env {envQParams = M.insert "offset" [T.show o] envQParams, ..}

withParam :: forall p. (Show p, Refl.Typeable p) => Text -> p -> EnvMod
withParam (T.map toLower -> pp) pv Env {..}
  | Just Refl.HRefl <- Refl.typeRep @p `Refl.eqTypeRep` Refl.typeRep @Text =
    Env {envQParams = M.insert pp [pv] envQParams, ..}
  | otherwise =
    Env {envQParams = M.insert pp [T.show pv] envQParams, ..}

instance HasDatabase (AppT TestMonad) where
  type Database (AppT TestMonad) = TestDB

  liftDatabase = handle (throwM . fromDBException) . lift

  getDatabaseConnection = pure ()

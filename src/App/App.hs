{-# OPTIONS_GHC -Wno-orphans #-}

module App.App
  ( runServer,
    Main,
  )
where

import Api.Article qualified as Article
import Api.Category qualified as Category
import Api.Delete ( delete_ )
import Api.Draft qualified as Draft
import Api.Get ( get_ )
import Api.Picture qualified as Picture
import Api.Post ( post_ )
import Api.ProtectedResources (protectedResources)
import Api.Publish ( publish_ )
import Api.Put ( put_ )
import Api.User qualified as User
import App.Config
    ( Config(Config, cAddress, cPort, cLogger, cDatabase) )
import App.Internal
    ( toPath,
      AppT,
      AppError(WrongPassword, AccessViolation, AdminAccessViolation,
               AlreadyExists, DatabaseOtherError, EntityNotFound, IsNull,
               ParsingError, PageNotFoundError, RequestHeadersError, QParamsError,
               Unathorized, UnknwonHTTPMethod),
      DB )
import App.QueryParams ( toQueryParams )
import App.Result ( AppResult(..) )
import App.Router
    ( put, get, post, newRouter, addMiddleware, runRouter, Routed(..) )
import App.Types ( getURL, Body )
import Control.Exception (IOException)
import Control.Monad.Catch ( handle )
import Control.Monad.Extra (whenM)
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy qualified as BL
import Data.Char ( toLower )
import Data.Kind ( Type )
import Data.String (fromString)
import Database.Database qualified as Database
import Entity.Article (Article)
import Entity.Author (Author)
import Entity.Category (Category)
import Entity.Draft (Draft)
import Entity.Picture ( Picture(Picture) )
import Entity.Tag (Tag)
import Entity.User (User)
import Extended.Text qualified as T
import Logger qualified
import Logger ((.<))
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Wai
import System.Environment ( getArgs )
import System.Exit qualified as Sys

runServer :: IO ()
runServer = handle handler $ do
  Config {..} <- BL.readFile "config.json" >>= parseOrFail
  connectionDB <- Database.mkConnectionIO @(DB IO) cDatabase
  let logger = Logger.runLogger @IO cLogger
  whenM (("migrations" `elem`) <$> getArgs) $
    Database.runMigrations @(DB IO) cDatabase logger
  Wai.run cPort $ \req respond -> do
    body <- Wai.strictRequestBody req
    logger Logger.Debug 
      $ T.take 1000 $ "Recieved request:\n" .< req
    ToResponse {..} <-
      toResponse
        <$> runRouter @Main
          logger
          connectionDB
          (toPath req)
          body
          (T.decodeUtf8 <$> lookup "Content-Type" (Wai.requestHeaders req))
          (toQueryParams $ Wai.queryString req)
          (T.decodeUtf8 <$> lookup "Authorization" (Wai.requestHeaders req))
          Config {..}
    respond $ Wai.responseLBS respStatus respHeaders respBody
  where
    handler (e :: IOException) = Sys.die $ show e <> ". Terminating..."
    parsingFail = fail . ("Parsing config error: " <>) . show
    parseOrFail = either parsingFail pure . eitherDecode
    toResponse = either responseFromError responseFromResult

data ToResponse = ToResponse
  { respStatus :: !HTTP.Status,
    respHeaders :: ![HTTP.Header],
    respBody :: !Body
  }
  deriving (Show)

responseFromResult :: AppResult -> ToResponse
responseFromResult = \case
  ResText t -> r200 textHeaders $ BL.fromStrict $ T.encodeUtf8 t
  ResJSON bl -> r200 jsonHeaders bl
  ResPicture (Picture contentType pic) -> r200 (pictureHeaders contentType) pic
  where
    r200 = ToResponse HTTP.status200
    toHeaders x = [("ContentType", x)]
    textHeaders = toHeaders "text/plain; charset=utf-8"
    jsonHeaders = toHeaders "application/json"
    pictureHeaders contentType =
      toHeaders $
        "image/"
          <> fromString (map toLower $ show contentType)

responseFromError :: AppError -> ToResponse
responseFromError = \case
  AccessViolation t -> r403 t
  AdminAccessViolation -> r404 "Page not found."
  AlreadyExists t -> r409 t
  DatabaseOtherError t -> r400 $ toBL t
  EntityNotFound t -> r404 t
  IsNull t -> r400 $ toBL t
  ParsingError t -> r400 $ toBL t
  PageNotFoundError path -> r404 $ T.intercalate "/" (getURL path) <> " doesn't exists!"
  RequestHeadersError t -> r400 $ toBL t
  QParamsError -> r400 $ toBL "Query parameters error."
  Unathorized t -> r401 $ toBL t
  UnknwonHTTPMethod method -> r405 $ "Method " <> T.show method <> " is not allowed."
  WrongPassword -> r403 "Wrong password."
  err -> r500 $ toBL $ "Internal error:" <> T.show err
  where
    r400 = ToResponse HTTP.status400 []
    r401 = ToResponse HTTP.status401 []
    r403 = ToResponse HTTP.status403 [] . fromString . T.unpack
    r404 = ToResponse HTTP.status404 [] . fromString . T.unpack
    r405 = ToResponse HTTP.status405 [] . fromString . T.unpack
    r409 = ToResponse HTTP.status409 [] . fromString . T.unpack
    r500 = ToResponse HTTP.internalServerError500 []
    toBL = BL.fromStrict . T.encodeUtf8

data Main :: Type -> Type

instance Routed Main (AppT IO) where
  router = do
    addMiddleware protectedResources
    newRouter @User
    newRouter @Author
    newRouter @Tag
    newRouter @Category
    newRouter @Article
    newRouter @Draft
    newRouter @Picture

instance Routed User (AppT IO) where
  router = do
    post "users" User.postUser
    get "users/me" User.getMe
    delete_ "admin/users/{ID}"
    post "auth" User.authUser

instance Routed Author (AppT IO) where
  router = do
    post_ "admin/authors"
    get_ "admin/authors"
    get_ "admin/authors/{ID}"
    put_ "admin/authors/{ID}"
    delete_ "admin/authors/{ID}"

instance Routed Tag (AppT IO) where
  router = do
    post_ "admin/tags"
    get_ "tags"
    get_ "tags/{ID}"
    put_ "admin/tags/{ID}"
    delete_ "admin/tags/{ID}"

instance Routed Category (AppT IO) where
  router = do
    post_ "admin/categories"
    get_ "categories"
    get_ "categories/{ID}"
    put "admin/categories/{ID}" Category.putCategory
    delete_ "admin/categories/{ID}"

instance Routed Article (AppT IO) where
  router = do
    get "articles" Article.getArticles
    get_ "articles/{ID}"

instance Routed Draft (AppT IO) where
  router = do
    addMiddleware Draft.draftAccess
    post "drafts" Draft.postDraft
    get "drafts" Draft.getDrafts
    get_ "drafts/{ID}"
    put_ "drafts/{ID}"
    delete_ "drafts/{ID}"
    publish_ "drafts/{ID}"

instance Routed Picture (AppT IO) where
  router = do
    post "pictures" Picture.postPicture
    get "pictures/{ID}" Picture.getPicture
    delete_ "pictures/{ID}"

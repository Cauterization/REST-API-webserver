{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module App.App 
    ( runServer
    , Main
    ) where

import App.Config

import Api.Put
import App.Result
import App.Router 
import App.Internal
import App.QueryParams
import App.Types
import Api.ProtectedResources (protectedResources)
import Api.Article qualified as Article
import Api.Author qualified as Author
import Api.Category qualified as Category
import Api.Draft qualified as Draft
import Api.User qualified as User
import Api.Picture qualified as Picture
import Api.Post
import Api.Get
import Api.Delete
import Api.Publish


import Control.Exception ( IOException ) 
import Control.Monad.Catch

import Data.Aeson (eitherDecode)
import Data.Kind
import Data.Char
import Data.ByteString.Lazy qualified as BL

import Extended.Text qualified as T

import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Wai

import System.Environment
import System.Exit qualified as Sys

import Database.Database qualified as Database

import Logger qualified
import Logger ((.<))
import Entity.Author (Author)
import Entity.Article (Article)
import Entity.Draft (Draft)
import Entity.Category (Category)
import Entity.Tag (Tag)
import Entity.User (User)
import Entity.Picture
import Postgres.Internal

import Control.Monad.Extra (whenM)
import qualified Network.HTTP.Types as HTTP

import Data.String (fromString)

runServer :: IO ()
runServer = handle handler $ do
    Config{..} <- BL.readFile "config.json" >>= parseOrFail
    connectionDB <- Database.mkConnectionIO @(DB IO) cDatabase
    let logger = Logger.runLogger @IO cLogger
    whenM (("migrations" `elem`) <$> getArgs) $
        Database.runMigrations @(DB IO) cDatabase logger
    Wai.run cPort $ \req respond -> do
        body <- Wai.strictRequestBody req
        ToResponse{..} <- toResponse <$> runRouterWith @Main
            logger 
            connectionDB
            (toPath req) 
            body
            (T.decodeUtf8 <$> lookup "Content-Type" (Wai.requestHeaders req))
            (toQueryParams $ Wai.queryString req)
            (T.decodeUtf8 <$> lookup "Authorization" (Wai.requestHeaders req))           
            Config{..}
            (Logger.debug $ T.take 1000 $ "Recieved request:\n" .< req)
        respond $ Wai.responseLBS respStatus respHeaders respBody
  where
    handler (e :: IOException) = Sys.die $ show e <> ". Terminating..."
    parsingFail = fail . ("Parsing config error: " <>) . show 
    parseOrFail = either parsingFail pure . eitherDecode
    toResponse = either responseFromError responseFromResult

data ToResponse = ToResponse 
    { respStatus  :: HTTP.Status
    , respHeaders :: [HTTP.Header] 
    , respBody    :: Body
    } deriving Show

responseFromResult :: AppResult -> ToResponse
responseFromResult = \case
    ResText t  -> r200 textHeaders $ BL.fromStrict $ T.encodeUtf8 t
    ResJSON bl -> r200 jsonHeaders bl
    ResPicture (Picture contentType pic) -> r200 (pictureHeaders contentType) pic
  where
    r200 = ToResponse HTTP.status200 
    toHeaders x = [("ContentType", x)]
    textHeaders = toHeaders "text/plain; charset=utf-8"
    jsonHeaders = toHeaders "application/json"
    pictureHeaders contentType = toHeaders $ "image/" 
        <> (fromString $ map toLower $ show contentType)

responseFromError :: AppError -> ToResponse
responseFromError = \case
    Err404 path      -> r404 $ T.concat (getURL path) <> " doesn't exists!"
    EntityNotFound     t -> r404 t
    AlreadyExists      t -> r409 t
    AccessViolation    t -> r403 t
    ParsingErr         t -> r400 $ toBL t
    DatabaseOtherError t -> r400 $ toBL t
    err -> r500 $ toBL $ "Internal error:" <> T.show err <> " (not handled yet)"
  where
    r400  = ToResponse HTTP.status400 []
    r403  = ToResponse HTTP.status403 [] . fromString . T.unpack
    r404  = ToResponse HTTP.status404 [] . fromString . T.unpack
    r409  = ToResponse HTTP.status409 [] . fromString . T.unpack
    r500  = ToResponse HTTP.internalServerError500 [] 
    toBL  = BL.fromStrict . T.encodeUtf8 

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
        post    "users"                     User.postUser
        get     "users/me"                  User.getMe
        delete_ "admin/users/{ID}" 
        post    "auth"                      User.authUser

instance Routed Author (AppT IO) where
    router = do
        post_   "admin/authors"         
        get_    "admin/authors"          
        get_    "admin/authors/{ID}" 
        put_    "admin/authors/{ID}"     
        delete_ "admin/authors/{ID}"  

instance Routed Tag (AppT IO) where
    router = do
        post_   "admin/tags"       
        get_    "tags"    
        get_    "tags/{ID}"    
        put_    "admin/tags/{ID}"
        delete_ "admin/tags/{ID}"

instance Routed Category (AppT IO) where
    router = do
        post_   "admin/categories"
        get_    "categories"
        put     "admin/categories/{ID}"      Category.putCategory     
        delete_ "admin/categories/{ID}"       

instance Routed Article (AppT IO) where
    router = do
        get      "articles"                  Article.getArticles
        get_     "articles/{ID}"    

instance Routed Draft (AppT IO) where
    router = do
        addMiddleware                        Draft.draftAccess
        post     "drafts"                    Draft.postDraft
        get      "drafts"                    Draft.getDrafts
        get_     "drafts/{ID}"              
        put_     "drafts/{ID}"              
        delete_  "drafts/{ID}"
        publish_ "drafts/{ID}"

instance Routed Picture (AppT IO) where
    router = do
        post     "pictures"                  Picture.postPicture
        get      "pictures/{ID}"             Picture.getPicture
        delete_  "pictures/{ID}"
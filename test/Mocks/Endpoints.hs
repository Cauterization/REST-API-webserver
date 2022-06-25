module Mocks.Endpoints where

import Api.Article qualified as Article
import Api.Category qualified as Category
import Api.Delete (delete_)
import Api.Draft qualified as Draft
import Api.Get (get_)
import Api.Picture qualified as Picture
import Api.Post (post_)
import Api.ProtectedResources (protectedResources)
import Api.Publish (publish_)
import Api.Put (put_)
import Api.User qualified as User
import App.Endpoints (Main)
import App.Result (AppResult (ResText), text)
import App.Router
  ( Routed (..),
    addMiddleware,
    get,
    newRouter,
    post,
    put,
  )
import Data.Kind (Type)
import Entity.Article (Article)
import Entity.Author (Author)
import Entity.Category (Category)
import Entity.Draft (Draft)
import Entity.Picture (Picture)
import Entity.Tag (Tag)
import Entity.User (User)
import Extended.Text (Text)
import Mocks.Entity.Article ()
import Mocks.Entity.Author ()
import Mocks.Entity.Category ()
import Mocks.Entity.Draft ()
import Mocks.Entity.Picture ()
import Mocks.TestMonad (TestMonadT)

instance Routed Main TestMonadT where
  router = do
    newRouter @User
    newRouter @Author
    newRouter @Tag
    newRouter @Category
    newRouter @Article
    newRouter @Picture
    newRouter @Draft
    newRouter @ProtectedResourcesTest
    newRouter @RouterTest

instance Routed User TestMonadT where
  router = do
    post "users" User.postUser
    get "users/me" User.getMe
    delete_ "users/{ID}"
    post "auth" User.authUser

instance Routed Author TestMonadT where
  router = do
    post_ "authors"
    get_ "authors"
    get_ "authors/{ID}"
    put_ "authors/{ID}"
    delete_ "authors/{ID}"

instance Routed Tag TestMonadT where
  router = do
    post_ "tags"
    get_ "tags"
    get_ "tags/{ID}"
    put_ "tags/{ID}"
    delete_ "tags/{ID}"

instance Routed Category TestMonadT where
  router = do
    post_ "categories"
    get_ "categories"
    get_ "categories/{ID}"
    put "categories/{ID}" Category.putCategory
    delete_ "categories/{ID}"

instance Routed Picture TestMonadT where
  router = do
    post "pictures" Picture.postPicture
    get "pictures/{ID}" Picture.getPicture
    delete_ "pictures/{ID}"

instance Routed Article TestMonadT where
  router = do
    get "articles" Article.getArticles
    get_ "articles/{ID}"

instance Routed Draft TestMonadT where
  router = do
    addMiddleware Draft.draftAccess
    post "drafts" Draft.postDraft
    get "drafts" Draft.getDrafts
    get_ "drafts/{ID}"
    put_ "drafts/{ID}"
    delete_ "drafts/{ID}"
    publish_ "drafts/{ID}"

data ProtectedResourcesTest :: Type -> Type

instance Routed ProtectedResourcesTest TestMonadT where
  router = do
    addMiddleware protectedResources
    post "admin/PRtest" $ pure $ text "ok"

data RouterTest :: Type -> Type

instance Routed RouterTest TestMonadT where
  router = do
    post "router ok test" $ pure $ text "ok"
    post "ambiguousPatterns test" $ pure $ text ""
    post "ambiguousPatterns test" $ pure $ text ""
    addMiddleware $ const $ pure $ ResText routerMiddlewareCorrect
    post "middleware test" $ pure $ text routerMiddlewareIncorrect

routerMiddlewareCorrect :: Text
routerMiddlewareCorrect = "it works"

routerMiddlewareIncorrect :: Text
routerMiddlewareIncorrect = "it doesn't works"

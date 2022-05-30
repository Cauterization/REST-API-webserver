{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module App.Endpoints where

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
import App.AppT (AppT)
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

data Main :: Type -> Type

instance Routed Main (AppT IO) where
  router = do
    addMiddleware protectedResources
    newRouter @User
    newRouter @Author
    newRouter @Tag
    newRouter @Category
    newRouter @Picture
    newRouter @Article
    newRouter @Draft

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

instance Routed Picture (AppT IO) where
  router = do
    post "pictures" Picture.postPicture
    get "pictures/{ID}" Picture.getPicture
    delete_ "pictures/{ID}"

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

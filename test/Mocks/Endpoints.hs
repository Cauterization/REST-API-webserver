{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Mocks.Endpoints where

import Api.Delete
import Api.Get
import Api.Post
import Api.Put
import Api.User qualified as User
import App.Endpoints
import App.Error
import App.Result
import App.Router
import Control.Monad.Catch
import Data.Kind
import Database.Internal
import Entity.Author
import Entity.Tag
import Entity.User
import Extended.Text (Text)
import Mocks.Entity.Author
import Mocks.Entity.Tag
import Mocks.TestMonad

instance Routed Main TestMonadT where
  router = do
    newRouter @User
    newRouter @Author
    newRouter @Tag
    -- newRouter @Category
    -- newRouter @Article
    -- newRouter @Picture
    -- newRouter @Draft
    newRouter @RouterTest

instance Routed User TestMonadT where
  router = do
    post "users" User.postUser
    -- get "users/me" User.getMe
    -- delete_ "admin/users/{ID}"
    -- post "auth" User.authUser

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

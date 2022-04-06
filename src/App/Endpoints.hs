{-# OPTIONS_GHC -Wno-orphans #-}

module App.Endpoints where


import Control.Monad.Identity
import Control.Monad.Catch ( MonadThrow(..), MonadCatch, try ) 
import Control.Monad.Writer
import Control.Monad.Reader

import Data.Coerce
import Data.Kind (Type)
import Network.Wai qualified as Wai

import App.Get

import Extended.Text qualified as T

import Database.Database qualified as Database

import Postgres.Internal

import Logger qualified



import App.App
import App.Run
import App.Internal
import App.Types
import App.Router
import App.QueryParams

import Entity.User
import Entity.Author
import App.Types (PaginationSize)

data Main :: Type -> Type


instance Routed Main Postgres where
    router = do
        newRouter @User 
        newRouter @Author 
        post "asdasd" $ pure $ text "asd"
        
instance Routed User Postgres where
    router = do
        --post_   "users"      
        get     "users/me"   getCurrentUser
        --delete_ "admin/users" 
        --post    "login"       loginUser

instance {-# OVERLAPPABLE #-} Routed Author Postgres where
    router = do
        --post_   "admin/authors"         
        get_    "admin/authors"          
        --get_    "admin/authors/{ID}" 
        --put_    "admin/authors/{ID}"     
        --delete_ "admin/authors/{ID}"  


runRouterWith :: forall m a.
    ( Monad m
    , MonadThrow m 
    , Logger.RunLogger m
    , MonadCatch m
    , Application (AppT m)
    , Routed Main (DB m)
    ) 
    => Logger.Logger m 
    -> Database.ConnectionOf (DB m)
    -> Path Current 
    -> Body 
    -> QueryParams
    -> PaginationSize
    -> AppT m a
    -> m ToResponse
runRouterWith logger connDB path body qparams pagSize with 
    = runApp (Env logger connDB path body qparams pagSize) $ with >> do
        execWriterT (runReaderT (unRouter (router @Main @(DB m))) path) 
            >>= \case
                Route _ success      -> success
                AmbiguousPatterns ps 
                    -> throwM $ OtherErr $ "AmbiguousPatterns" <> T.show ps
                _                    -> throw404


-- mkEnv :: Logger.RunLogger m 
--     => Logger.Config 
--     -> Path Current 
--     -> Wai.Request 
--     -> Body 
--     -> Database.ConnectionOf (Database.Database (AppT m)) 
--     -> Env m
-- mkEnv lConf path req body = 
--         Env (Logger.runLogger lConf) path body (toQParams $ Wai.queryString req)
module Api.ProtectedResources where

import Control.Monad.Extra
import Control.Monad.Catch
import Control.Monad.Reader

import Entity.User
import Entity.Internal

-- import Extended.Text qualified as T
import Data.List

import Logger qualified

import HKD.HKD

import Database.Database qualified as Database

import Api.User qualified as User
import Api.Get 

import App.Router
import App.Internal
import App.Types

protectedResources :: 
    ( Application m
    , Gettable m (Entity User) Display
    , Database.ToRowOf (Database.Database m) [Token]
    ) =>  Middleware m
protectedResources = (whenM protectedRequest adminCheck >>)

protectedRequest :: HasEnv m => m Bool
protectedRequest =  asks (isPrefixOf ["admin"] . getURL . envPath)

adminCheck :: 
    ( Application m
    , Gettable m (Entity User) Display
    , Database.ToRowOf (Database.Database m) [Token]
    ) => m ()
adminCheck = do
    Logger.info "Attempt to auth admin..."
    Entity{entity = User{..}} <- catch (User.getCurrentUser @Display) handler
    unless admin $ throwM err
  where
    err = AdminAccessViolation "Admin check."
    handler = \case
        Database.EntityNotFound{} -> throwM err
        e -> throwM e
    
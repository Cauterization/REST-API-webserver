module Api.ProtectedResources where

import Control.Monad.Extra
import Control.Monad.Catch
import Control.Monad.Reader

import Entity.User

-- import Extended.Text qualified as T
import Data.List

import Logger qualified

import HKD.HKD

import Database.Database qualified as Database

import Api.User qualified as User

import App.Router
import App.Internal
import App.Types

protectedResources :: 
    ( Application m
    , Database.GettableFrom (Database.Database m) User Display
    , Database.FromRowOf    (Database.Database m) (User Display)
    , Database.ToRowOf      (Database.Database m) [Token]
    ) => Middleware m
protectedResources = (whenM protectedRequest adminCheck >>)

protectedRequest :: HasEnv m => m Bool
protectedRequest =  asks (isPrefixOf ["admin"] . getURL . envPath)

adminCheck :: 
    ( Application m
    , Database.GettableFrom (Database.Database m) User Display
    , Database.FromRowOf    (Database.Database m) (User Display)
    , Database.ToRowOf      (Database.Database m) [Token]
    ) => m ()
adminCheck = do
    Logger.info "Attempt to auth admin..."
    User{..} <- catch (User.getMe @Display []) handler
    unless admin $ throwM err
  where
    err = AccessViolation "Admin check."
    handler = \case
        Database.EntityNotFound{} -> throwM err
        e -> throwM e
    
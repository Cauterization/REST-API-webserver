{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module Api.ProtectedResources where

import Api.Get (Gettable)
import Api.User qualified as User
import App.AppT (Application, Env (envPath), HasEnv)
import App.Error (adminAccessViolationError)
import App.Error qualified as Database
import App.Path (getURL)
import App.Router (Middleware)
import App.Types (Token)
import Control.Monad.Catch
  ( MonadCatch (catch),
    MonadThrow (throwM),
  )
import Control.Monad.Extra (unless, whenM)
import Control.Monad.Reader (asks)
import Data.List (isPrefixOf)
import Database.HasDatabase qualified as Database
import Entity.Internal (Entity (Entity, entity))
import Entity.User (User (..))
import HKD.HKD (Display)
import Logger qualified

protectedResources ::
  ( Application m,
    Gettable m (Entity User) Display,
    Database.ToRowOf m [Token]
  ) =>
  Middleware m
protectedResources = (whenM protectedRequest adminCheck >>)

protectedRequest :: HasEnv m => m Bool
protectedRequest = asks (isPrefixOf ["admin"] . getURL . envPath)

adminCheck ::
  ( Application m,
    Gettable m (Entity User) Display,
    Database.ToRowOf m [Token]
  ) =>
  m ()
adminCheck = do
  Logger.info "Attempt to auth admin..."
  Entity {entity = User {..}} <- catch (User.getCurrentUser @Display) handler
  unless admin adminAccessViolationError
  where
    handler = \case
      Database.EntityNotFound {} -> adminAccessViolationError
      e -> throwM e

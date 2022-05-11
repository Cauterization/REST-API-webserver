module Api.ProtectedResources where

import Api.Get (Gettable)
import Api.User qualified as User
import App.Internal
  ( AppError (AdminAccessViolation),
    Application,
    Env (envPath),
    HasEnv,
  )
import App.Router (Middleware)
import App.Types (Token, getURL)
import Control.Monad.Catch
  ( MonadCatch (catch),
    MonadThrow (throwM),
  )
import Control.Monad.Extra (unless, whenM)
import Control.Monad.Reader (asks)
import Data.List (isPrefixOf)
import Database.Database qualified as Database
import Entity.Internal (Entity (Entity, entity))
import Entity.User (User (..))
import HKD.HKD (Display)
import Logger qualified

protectedResources ::
  ( Application m,
    Gettable m (Entity User) Display,
    Database.ToRowOf (Database.Database m) [Token]
  ) =>
  Middleware m
protectedResources = (whenM protectedRequest adminCheck >>)

protectedRequest :: HasEnv m => m Bool
protectedRequest = asks (isPrefixOf ["admin"] . getURL . envPath)

adminCheck ::
  ( Application m,
    Gettable m (Entity User) Display,
    Database.ToRowOf (Database.Database m) [Token]
  ) =>
  m ()
adminCheck = do
  Logger.info "Attempt to auth admin..."
  Entity {entity = User {..}} <- catch (User.getCurrentUser @Display) handler
  unless admin $ throwM err
  where
    err = AdminAccessViolation "Admin check."
    handler = \case
      Database.EntityNotFound {} -> throwM err
      e -> throwM e

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Test.App where

import Control.Monad.Catch
    ( Exception(fromException),
      SomeException(SomeException),
      MonadCatch(..),
      MonadThrow(..) )
import Control.Monad.Except
    ( join, when, ExceptT(..), runExceptT, MonadError(throwError) )
import Control.Monad.Extra (whenM)
import Control.Monad.State
import Control.Monad.Writer 

import Data.Text (Text)

import Data.Map qualified as M

import Logger qualified

import App.Internal
import App.Types

import Database.Database
import Data.Maybe
import Data.Coerce


newtype TestMonad e a = TestMonad
    { unTestM :: WriterT [(Logger.Verbosity, Text)] 
               (ExceptT AppError
               (State (TestState e))) a} 
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadWriter [(Logger.Verbosity, Text)] 
        , MonadError AppError
        , MonadState (TestState e)
        )

instance MonadThrow (TestMonad e) where
    throwM e = case fromException (SomeException e) of
        Just err -> throwError err
        _        -> error "non-bot exception."

instance MonadCatch (TestMonad e) where
    catch = undefined

instance Logger.HasLogger (TestMonad f) where
    mkLog v t = when (v >= Logger.Warning) 
              $ tell [(v, t)]

data TestState a = TState
    { tDB :: M.Map (ID a) a
    }

instance HasDatabase (AppT (TestMonad e)) where

    type Database (AppT (TestMonad e)) = M.Map (ID e) e

-- class ( IsDatabase (Database m)
--       ) 
--     => HasDatabase m where

--     type family Database (m :: Type -> Type) :: Type

--     liftDatabase :: DatabaseMonad (Database m) a -> m a

--     getDatabaseConnection :: m (ConnectionOf (Database m))

--     getPaginationSize :: m PaginationSize 

instance IsDatabase (M.Map (ID e) e) where

    type QueryOf       (M.Map (ID e) e) = ()
    type ToRowOf       (M.Map (ID e) e) q = q ~ e
    type FromRowOf     (M.Map (ID e) e) r = r ~ e
    type ConnectionOf  (M.Map (ID e) e)   = ()
    type DatabaseMonad (M.Map (ID e) e)   = TestMonad e

    runMigrations _ _ = pure ()

    mkConnectionIO _ = pure ()

    postToDatabase _ _ e = modify (\TState{..} 
        -> TState{tDB = M.insert (ID $ length tDB + 1) e tDB, ..}) 

    getFromDatabase _ _ q = gets (M.lookup (undefined q) . tDB) 
        >>= maybe (throwM EntityNotFound) (pure . pure)

    putIntoDatabase _ _ e = modify (\TState{..} 
        -> TState{tDB = M.insert (ID $ length tDB + 1) e tDB, ..}) 

    deleteFromDatabase _ _ [eID] = join $ gets $ \TState{..} 
        -> case M.lookup (coerce eID) tDB of
            Just e -> modify (\TState{..} 
                -> TState{tDB = M.delete (coerce eID) tDB,  ..}) >> pure 1
            Nothing -> pure 0

defaultEnv :: Env (TestMonad e)
defaultEnv  = Env 
    { envLogger = \v t -> when (v >= Logger.Warning) $ tell [(v, t)]
    , envConn = ()
    }




-- data Env (m :: Type -> Type) = Env
--     { envLogger     :: Logger.Logger m
--     , envConn       :: Database.ConnectionOf (Database.Database (AppT m))
--     , envPath       :: Path Current
--     , envBody       :: Body
--     , envQParams    :: QueryParams
--     , envToken      :: Maybe Token
--     , envPagination :: PaginationSize
--     }
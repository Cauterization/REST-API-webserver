{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App.Router where

import App.AppT
  ( AppT,
    Application,
    Env (Env),
    runApp,
  )
import App.Config (Config)
import App.Error (AppError, ambiguousPatterns, pageNotFoundError)
import App.Path
  ( Current,
    Path (DELETE, GET, POST, PUBLISH, PUT),
    Pattern,
    getMethod,
    getURL,
  )
import App.QueryParams (QueryParams)
import App.Result (AppResult, Endpoint)
import App.Types (Body, ContentType, ID, Token)
import Control.Monad.Catch (MonadCatch, try)
import Control.Monad.Reader
  ( MonadReader (ask),
    ReaderT (..),
  )
import Control.Monad.Writer
  ( MonadWriter (tell),
    WriterT,
    execWriterT,
    zipWithM,
  )
import Data.Coerce (coerce)
import Data.Kind (Type)
import Database.HasDatabase qualified as Database
import Extended.Text (Text)
import Extended.Text qualified as T
import HKD.HKD (Create, Delete, Display, Front, Publish, Update)
import Logger qualified

type Middleware m = m AppResult -> m AppResult

data RouterResult m
  = NotMatched
  | Route (Path Pattern) (m AppResult)
  | Middleware (Middleware m)
  | AmbiguousPatterns [Path Pattern]

instance Semigroup (RouterResult m) where
  NotMatched <> a = a
  a <> NotMatched = a
  Route pp r <> Middleware _ = Route pp r
  Middleware m <> Route pp r = Route pp $ m r
  Middleware m1 <> Middleware m2 = Middleware $ m2 . m1
  Route pp1 _ <> Route pp2 _ = AmbiguousPatterns [pp1, pp2]
  AmbiguousPatterns pps <> Route pp _ = AmbiguousPatterns $ pp : pps
  Route pp _ <> AmbiguousPatterns pps = AmbiguousPatterns $ pp : pps
  AmbiguousPatterns pps <> _ = AmbiguousPatterns pps
  _ <> AmbiguousPatterns pps = AmbiguousPatterns pps

instance Monoid (RouterResult m) where
  mempty = NotMatched

newtype Router (e :: Type -> Type) (m :: Type -> Type) a = Router
  {unRouter :: ReaderT (Path Current) (WriterT (RouterResult m) m) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader (Path Current),
      MonadWriter (RouterResult m)
    )

runRouter ::
  forall e m.
  ( Application (AppT m),
    Routed e (AppT m),
    Monad m,
    MonadCatch m
  ) =>
  Logger.Logger m ->
  Database.ConnectionOf (AppT m) ->
  Path Current ->
  Body ->
  Maybe ContentType ->
  QueryParams ->
  Maybe Token ->
  Config ->
  m (Either AppError AppResult)
runRouter logger connDB path body conentType qparams token config =
  try $
    runApp (Env logger connDB path body conentType qparams token config) $
      execWriterT (runReaderT (unRouter (router @e @(AppT m))) path)
        >>= \case
          Route _ success -> success
          AmbiguousPatterns ps -> ambiguousPatterns ps
          _ -> pageNotFoundError

withPathCmp :: Endpoint m -> Path Pattern -> Path Current -> RouterResult m
withPathCmp f pp pc
  | sameMethod && sameLength && correctPath =
    maybe mempty (Route pp . f) $
      concat
        <$> zipWithM cmpSegment (getURL pp) (getURL pc)
  | otherwise = mempty
  where
    sameMethod = getMethod pp == getMethod pc
    sameLength = cmpLength pp == cmpLength pc
    correctPath = (not . any (any (`elem` ['{', '}']) . T.unpack)) $ getURL pc
    cmpLength = length . getURL
    cmpSegment "{ID}" (T.read @(ID (Path Current)) -> Right n) = Just [n]
    cmpSegment r ((== r) -> True) = Just []
    cmpSegment _ _ = Nothing

addMiddleware :: Monad m => Middleware m -> Router e m ()
addMiddleware = tell . Middleware

addRoute ::
  Monad m =>
  Path Pattern ->
  Endpoint m ->
  Router (e :: Type -> Type) m ()
addRoute path endpoint = ask >>= tell . withPathCmp endpoint path

post,
  get,
  put,
  delete,
  publish ::
    forall (e :: Type -> Type) (m :: Type -> Type).
    Application m =>
    Text ->
    Endpoint m ->
    Router e m ()
post p ep = addRoute (POST $ T.splitOn "/" p) $ ep @e @Create
get p ep = addRoute (GET $ T.splitOn "/" p) $ ep @e @(Front Display)
put p ep = addRoute (PUT $ T.splitOn "/" p) $ ep @e @Update
delete p ep = addRoute (DELETE $ T.splitOn "/" p) $ ep @e @Delete
publish p ep = addRoute (PUBLISH $ T.splitOn "/" p) $ ep @e @Publish

class Routed e m where
  router :: Application m => Router e m ()

newRouter ::
  forall e m a.
  ( Application m,
    Routed e m
  ) =>
  Router a m ()
newRouter = coerce (router @e @m)

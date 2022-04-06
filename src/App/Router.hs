{-# LANGUAGE AllowAmbiguousTypes
           , PolyKinds
           , ViewPatterns   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App.Router where

import Control.Monad.Reader
import Control.Monad.Catch
import Control.Monad.Writer
import Control.Monad.Identity

import Data.Coerce
import Data.Kind (Type)


import Extended.Text(Text)
import Extended.Text qualified as T

import App.Result

import App.Types

import Database.Database qualified as Database
import Database.Database (Database)

import Postgres.Internal (Postgres)

import App.Config
import App.Result
import App.Run
import App.Internal
import Network.Wai qualified as Wai
-- import Server.Base
-- import Server.App
-- import Data.Aeson
import HKD.HKD

import Logger qualified
import App.QueryParams

type Middleware m a = m a -> m a

data RouterResult m
    = NotMatched
    | Route (Path Pattern) (m AppResult)
    | Middleware (Middleware m AppResult)
    | AmbiguousPatterns [Path Pattern]

instance Semigroup (RouterResult m) where
    NotMatched <> a  = a
    a <>  NotMatched = a
    Route pp r    <> Middleware _  = Route pp r
    Middleware m  <> Route pp r    = Route pp  $ m r
    Middleware m1 <> Middleware m2 = Middleware $ m2 . m1
    Route pp1 _  <> Route pp2 _         = AmbiguousPatterns [pp1, pp2]
    AmbiguousPatterns pps <> Route pp _ = AmbiguousPatterns $ pp : pps
    Route pp _ <> AmbiguousPatterns pps = AmbiguousPatterns $ pp : pps
    AmbiguousPatterns pps <> _          = AmbiguousPatterns pps
    _ <> AmbiguousPatterns pps          = AmbiguousPatterns pps

instance Monoid (RouterResult m) where
    mempty = NotMatched

newtype Router (e :: Type -> Type) (m :: Type -> Type) a 
    = Router { unRouter :: ReaderT (Path Current) (WriterT (RouterResult m) m) a }
    deriving newtype (Functor, Applicative, Monad, MonadReader (Path Current)
        , MonadWriter (RouterResult m))

withPathCmp :: Endpoint m -> Path Pattern -> Path Current -> RouterResult m
withPathCmp f pp pc
    | sameMethod && sameLength && correctPath
        = maybe mempty (Route pp . f) $ concat 
            <$> zipWithM cmpSegment (getURL pp) (getURL pc)
    | otherwise = mempty
  where
    sameMethod = getMethod pp == getMethod pc
    sameLength = cmpLength pp == cmpLength pc
    correctPath = (not . any (any (`elem` ['{', '}']) . T.unpack)) $ getURL pc
    cmpLength = length . getURL
    cmpSegment "{ID}" (T.read @(ID Path) -> Right n) = Just [n]
    cmpSegment r      ((== r) -> True)               = Just []
    cmpSegment _      _                              = Nothing

type Application (m :: Type -> Type) =
    ( MonadThrow m
    , MonadCatch m
    , HasEnv m
    , Logger.HasLogger m
    , Database.ToRowOf (Database.Database m) IDs
    , Database.ToRowOf (Database.Database m) [Page]
    , Database.HasDatabase m
    )

type Gettable m e a =
    ( Database.GettableFrom (Database.Database m) e a
    )

addRoute :: Monad m
    => Path Pattern 
    -> Endpoint m
    -> Router (e :: Type -> Type) m ()
addRoute pp f = ask >>= tell . withPathCmp f pp

post, get, put, delete :: forall (e :: Type -> Type) (m :: Type -> Type). Monad m
    => Text 
    -> (forall (e1 :: Type -> Type) (e2 :: Type). Endpoint m)
    -> Router e m ()

post   p f = addRoute (POST   $ T.splitOn "/" p) $ f @e @Create
get    p f = addRoute (GET    $ T.splitOn "/" p) $ f @e @Display
put    p f = addRoute (PUT    $ T.splitOn "/" p) $ f @e @Update
delete p f = addRoute (DELETE $ T.splitOn "/" p) $ f @e @Delete

class Routed e db where
    router :: forall m. 
        ( Database.Database m ~ db
        , Application m
        ) => Router e m ()

newRouter :: forall e m a. 
    ( Application m
    , Routed e (Database.Database m)
    ) => Router a m ()
newRouter = coerce (router @e @(Database.Database m) @m)


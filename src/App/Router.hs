{-# LANGUAGE AllowAmbiguousTypes
           , PolyKinds
           , ViewPatterns   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App.Router where

import Control.Monad.Reader
import Control.Monad.Catch
import Control.Monad.Writer
import Control.Monad.Identity

import Data.Aeson

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
import Data.Data (Data, Typeable)

type Middleware m = m AppResult -> m AppResult

data RouterResult m
    = NotMatched
    | Route (Path Pattern) (m AppResult)
    | Middleware (Middleware m)
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

runRouterWith :: forall main m a.
    ( Monad m
    , MonadThrow m 
    , Logger.RunLogger m
    , MonadCatch m
    , Application (AppT m)
    , Routed main (DB m)
    ) 
    => Logger.Logger m 
    -> Database.ConnectionOf (DB m)
    -> Path Current 
    -> Body 
    -> QueryParams
    -> Maybe Token
    -> PaginationSize
    -> AppT m a
    -> m ToResponse
runRouterWith logger connDB path body qparams token pagSize with 
    = runApp (Env logger connDB path body qparams token pagSize) $ with >> do
        execWriterT (runReaderT (unRouter (router @main @(DB m))) path) 
            >>= \case
                Route _ success      -> success
                AmbiguousPatterns ps -> ambiguousPatterns ps
                _                    -> throw404


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
    cmpSegment "{ID}" (T.read @(ID (Path Current)) -> Right n) = Just [n]
    cmpSegment r      ((== r) -> True)               = Just []
    cmpSegment _      _                              = Nothing

type Application (m :: Type -> Type) = 
    ( MonadThrow m
    , MonadCatch m
    , HasEnv m
    , Impure m
    , Logger.HasLogger m
    , Database.ToRowOf (Database.Database m) IDs
    , Database.ToRowOf (Database.Database m) [Page]
    , Database.ToRowOf (Database.Database m) [Token]
    , Database.HasDatabase m
    , Database.QConstraints (Database.Database m)
    )

type Postable m e =
    ( Database.PostableTo (Database m) e
    , Database.ToRowOf (Database m) (e Create)
    , Database.FromRowOf (Database m) (ID (e Create))
    , Show (ID (e Create))
    , Data (e Create)
    , Show (e Create)
    , Typeable e
    )

type Gettable m e a =
    ( Database.GettableFrom (Database.Database m) e a
    , Database.FromRowOf (Database m) (e a)
    , Data (e a)
    , Show (e a)
    , Typeable e
    )

type Puttable m e a =
    ( Database.PuttableTo (Database.Database m) e 
    , Database.ToOneRow (e (Front Update)) IDs
    , Database.ToRowOf (Database.Database m) (Database.MkOneRow (e (Front Update)) IDs)
    , Data (e a)
    , Typeable e
    )

type Deletable m e =
    ( Database.DeletableFrom (Database m) e
    -- , Database.ToRowOf (Database m) (e Create)
    -- , Database.FromRowOf (Database m) (ID (e Create))
    -- , Show (ID (e Create))
    , Data (e Delete)
    -- , Show (e Create)
    , Typeable e
    )

addMiddleware ::Monad m => Middleware m -> Router e m ()
addMiddleware = tell . Middleware

addRoute :: Monad m
    => Path Pattern 
    -> Endpoint m
    -> Router (e :: Type -> Type) m ()
addRoute pp f = ask >>= tell . withPathCmp f pp

post, get, put, delete :: forall (e :: Type -> Type) (m :: Type -> Type). Application m
    => Text 
    -> Endpoint m
    -> Router e m ()

post   p ep = addRoute (POST   $ T.splitOn "/" p) $ ep @e @Create
get    p ep = addRoute (GET    $ T.splitOn "/" p) $ ep @e @(Front Display)
put    p ep = addRoute (PUT    $ T.splitOn "/" p) $ ep @e @Update
delete p ep = addRoute (DELETE $ T.splitOn "/" p) $ ep @e @Delete

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


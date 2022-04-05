{-# LANGUAGE AllowAmbiguousTypes
           , PolyKinds
           , ViewPatterns   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Server.Router where

import Control.Monad.Reader
import Control.Monad.Writer

import Data.Coerce
import HKD.Display

import Extended.Text(Text)
import Extended.Text qualified as T

import Types

import Database.Database (Database)

import Server.Base
import Server.App

type Middleware m a = m a -> m a

data RouterResult m
    = NotMatched
    | Route (Path Pattern) (m AppResult)
    | Middleware (Middleware m AppResult)
    | AmbiguousPatterns [Path Pattern]

instance Semigroup (RouterResult m) where
    NotMatched <> a  = a
    a <>  NotMatched = a
    Route pp r    <> Middleware m  = Route pp r
    Middleware m  <> Route pp r    = Route pp  $ m r
    Middleware m1 <> Middleware m2 = Middleware $ m2 . m1
    Route pp1 r1  <> Route pp2 r2       = AmbiguousPatterns [pp1, pp2]
    AmbiguousPatterns pps <> Route pp _ = AmbiguousPatterns $ pp : pps
    Route pp _ <> AmbiguousPatterns pps = AmbiguousPatterns $ pp : pps
    AmbiguousPatterns pps <> _          = AmbiguousPatterns pps
    _ <> AmbiguousPatterns pps          = AmbiguousPatterns pps

instance Monoid (RouterResult m) where
    mempty = NotMatched

newtype Router (e :: * -> *) (m :: * -> *) a 
    = Router { unRouter :: ReaderT (Path Current) (WriterT (RouterResult m) m) a }
    deriving newtype (Functor, Applicative, Monad, MonadReader (Path Current)
        , MonadWriter (RouterResult m))

withPathCmp :: Endpoint m -> Path Pattern -> Path Current -> RouterResult m
withPathCmp f pp pc
    | sameMethod && sameLength && correctPath
        = maybe mempty (Route pp . f) $ concat <$> zipWithM cmpSegment (getURL pp) (getURL pc)
    | otherwise = mempty
  where
    sameMethod = getMethod pp == getMethod pc
    sameLength = cmpLength pp == cmpLength pc
    correctPath = (not . any (any (`elem` ['{', '}']) . T.unpack)) $ getURL pc
    cmpLength = length . getURL
    cmpSegment "{ID}" (T.read @(ID Path) -> Right n) = Just [n]
    cmpSegment r      ((== r) -> True)               = Just []
    cmpSegment _      _                              = Nothing

type Poly a = forall (entity :: * -> *) (action :: *). a

addRoute :: Monad m
    => Path Pattern 
    -> Poly (Endpoint m) 
    -> Router (e :: * -> *) m ()
addRoute pp f = ask >>= tell . withPathCmp f pp

get :: forall (e :: * -> *) (m :: * -> *). Monad m
    => Text 
    -> Poly (Endpoint m)
    -> Router e m ()
get p f = addRoute (GET $ T.splitOn "/" p) $ f @e @Display

get_ :: forall (e :: * -> *) (m :: * -> *). 
    ( Entity (Database m) e
    , Application m
    ) => Text -> Router e m ()
get_ p = get p (getE @e)
{-}
post, get, put, delete :: forall (e :: * -> *) (m :: * -> *). Monad m
    => Text 
    -> (forall (e1 :: * -> *) (e2 :: *). Endpoint m)
    -> Router e m ()

post   p f = addRoute (POST   p) $ f @e @Create
get    p f = addRoute (GET    p) $ f @e @Display
put    p f = addRoute (PUT    p) $ f @e @Update
delete p f = addRoute (DELETE p) $ f @e @Delete

postE :: forall (e :: * -> *) m. 
    (FromJSON (e (Front Create))
    , Typeable e
    , Monad m
    , Logger.HasLogger (Appm)) 
    => [ID Path] -> m AppResult
postE ids = do
    Logger.info $ "Attempt to post " <> nameE @e
    e <- decodedBody @(e (Front Create))
    -- Database.postE e
    Logger.info $ nameE @e <> " successfully created."
    text "post e"
-}

{-}
getEByID :: forall (e :: * -> *) m db. (Monad m, MonadReader (Env db) m) => [ID Path] -> m AppResult
getEByID ids = do
    text "get e by id"

putE :: forall (e :: * -> *) m db. 
    (Monad m, MonadReader (Env db) m) => [ID Path] -> m AppResult
putE ids = do
    text "put entity"

deleteE :: forall (e :: * -> *) m db. 
    (Monad m, MonadReader (Env db) m) => [ID Path] -> m AppResult
deleteE ids = do
    text "delete entity"

post_ :: forall (e :: * -> *) m db. 
    (FromJSON (e (Front Create)), Typeable e, Monad m) 
    => Text                            
    -> Router e m ()
post_   p     = addRoute (POST   p) (postE    @e)

get_ :: forall (e :: * -> *) m db. 
    (ToJSON (e Display)
    , Database.DBEntity e
    , Typeable e
    , HasDatabase m
    , Database.FromRow (Database m) (e Display)
    , Monad m)
    => Text 
    -> Router e m ()
get_    p 
  | singleE p = addRoute (GET    p) (getEByID @e)
  | otherwise = addRoute (GET    p) (getE     @e)

put_, delete_ :: forall (e :: * -> *) m db. 
    (Monad m) 
    => Text 
    -> Router e m ()
put_    p     = addRoute (PUT    p) (putE     @e)
delete_ p     = addRoute (DELETE p) (deleteE  @e)

singleE :: Text -> Bool
singleE = T.isSuffixOf "{ID}"   
-}
class Routed e db where
    router :: forall m. 
        ( Database m ~ db
        , Application m
        ) => Router e m ()

newRouter :: forall e m a. 
    ( Application m
    , Routed e (Database m)
    ) => Router a m ()
newRouter = coerce (router @e @(Database m) @m)

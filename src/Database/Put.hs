{-# LANGUAGE UndecidableSuperClasses #-}
-- {-# LANGUAGE NoMonomorphismRestriction #-}
module Database.Put where

import Control.Monad.Catch
import Control.Monad

import Data.Kind
import Data.Data
import Entity.Internal qualified as Entity
import Entity.Internal (Entity(..))

import HKD.HKD

import Database.Internal
import Data.String
import Database.HasDatabase
import qualified Logger
import App.Types
import Data.List (intercalate)

import Extended.Text qualified as T

class Puttable (e :: Type) where

    putQuery :: (IsString s, Monoid s) => s

instance {-# OVERLAPPABLE #-} 
    ( Typeable e
    , Data (e a)
    ) => Puttable (Entity e a) where

    putQuery = mconcat
        [ "UPDATE " , fromString $ withPluralEnding $ nameOf @e
        , " SET "
        ,  toCoalesce $ fieldsOf @(e a)
        , " WHERE id = ? "
        ]

toCoalesce :: (IsString a) => [String] -> a
toCoalesce = fromString . intercalate ", " 
           . map (\s -> s <> " = COALESCE (?, " <> s <> ")") 


-- putEntity :: forall e (m :: Type -> Type) a.
--     ( HasDatabase m
--     , IsDatabase (Database m)
--     , Monad m
--     , MonadThrow m
--     , Logger.HasLogger m
--     , PuttableTo (Database m) e
--     , QConstraints (Database m)
--     , ToRowOf (Database m) (MkOneRow (e a) IDs)
--     , ToOneRow (e a) IDs
--     ) => IDs -> e a -> m ()
-- putEntity eID e = do
--     connection <- getDatabaseConnection 
--     let q = unQuery $ putQuery @(Database m) @e
--     Logger.sql q
--     row <- toOneRow e eID
--     liftDatabase $ putIntoDatabase @(Database m) @(MkOneRow (e a) IDs)
--         connection q row

putEntity :: forall e (m :: Type -> Type) a.
    ( HasDatabase m
    , Monad m
    , MonadThrow m
    , Logger.HasLogger m
    , ToRowOf (Database m) (Entity e a)
    , Puttable (Entity e a)
    , QConstraints (Database m)
    , Typeable e
    ) => Entity.Entity e a -> m ()
putEntity e = do
    connection <- getDatabaseConnection 
    let q = putQuery @(Entity e a)
    Logger.sql q
    res <- liftDatabase (putIntoDatabase @(Database m) @(Entity e a) connection q e) 
    when (res == 0) $ throwM $ EntityNotFound $ nameOf @e <> " not found."

putEntityWith :: forall x (m :: Type -> Type).
    ( HasDatabase m
    , IsDatabase (Database m)
    , Monad m
    , MonadThrow m
    , Logger.HasLogger m
    , Puttable x
    , QConstraints (Database m)
    , ToRowOf (Database m) x
    ) => x -> m Integer
putEntityWith x = do
    connection <- getDatabaseConnection 
    let q = putQuery @x
    Logger.sql q
    liftDatabase $ putIntoDatabase @(Database m) 
        connection q x

-- class ToOneRow a b where

--     type family MkOneRow a b :: Type 

--     toOneRow :: MonadThrow m => a -> b -> m (MkOneRow a b)




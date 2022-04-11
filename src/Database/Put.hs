module Database.Put where

import Control.Monad.Catch

import Data.Kind
import Data.Data
import Entity.Internal qualified as Entity

import HKD.HKD

import Database.Internal
import Database.Query
import Data.String
import Database.HasDatabase
import qualified Logger
import App.Types (IDs)
import Data.List (intercalate)

class PuttableTo db (e :: Type -> Type) where 

    putQuery :: Query db (e Update)

instance {-# OVERLAPS #-} (Data (e Update), Typeable e
    , ToRowOf db (MkOneRow (e (Front Update)) IDs)
    , QConstraints db)
    => PuttableTo db e where

    putQuery = mconcat
        [ "UPDATE " , fromString $ Entity.nameOf @e, "s "
        , "SET "
        , fromString $ intercalate ", " $ map fieldToCoalesce  
            $ Entity.fieldsOf @(e Update)
        , " WHERE id = ? "
        ]

fieldToCoalesce :: String -> String
fieldToCoalesce str =  str <> " = COALESCE (?, " <> str <> ")"

putEntity :: forall e (m :: Type -> Type) a.
    ( HasDatabase m
    , IsDatabase (Database m)
    , Monad m
    , MonadThrow m
    , Logger.HasLogger m
    , PuttableTo (Database m) e
    -- , ToRowOf (Database m) (e Update)
    , QConstraints (Database m)
    , ToRowOf (Database m) (MkOneRow (e a) IDs)
    , ToOneRow (e a) IDs
    ) => IDs -> e a -> m ()
putEntity eID e = do
    connection <- getDatabaseConnection 
    let q = unQuery $ putQuery @(Database m) @e
    Logger.sql q
    row <- toOneRow e eID
    liftDatabase $ putIntoDatabase @(Database m) @(MkOneRow (e a) IDs)
        connection q row

class ToOneRow a b where

    type family MkOneRow a b :: Type 

    toOneRow :: MonadThrow m => a -> b -> m (MkOneRow a b )




{-# LANGUAGE UndecidableSuperClasses #-}

module Database.Get where

import Data.Kind ( Type )
import Database.Internal
import Database.HasDatabase
import Database.Query
import Control.Monad.Identity

import App.Types

import Entity.Internal qualified as Entity
import Data.String
import Data.Data

import qualified Logger

class FromRowOf db (e a) =>
    GettableFrom db (e :: Type -> Type) a where 

    getQuery :: Query db (e a)

instance {-# OVERLAPS #-} (QConstraints db, Data (e a), Typeable e
    , FromRowOf db (e a)) 
    => GettableFrom db e a where

    getQuery = qSELECT (fromString $ Entity.fieldsQuery @(e a)) 
              <> qFROM   (fromString $ Entity.nameOf @e <> "s_view ")


getEntity :: forall e a m. 
    ( HasDatabase m
    , Monad m
    , Logger.HasLogger m
    , IsDatabase (Database m)
    , GettableFrom (Database m) e a
    , ToRowOf (Database m) [ID Path]
    , FromRowOf (Database m) (e a)
    , QConstraints (Database m)
    , Data (e a)
    , Typeable e 
    ) => 
    [ID Path] -> m (e a)
getEntity eIDs = do
    connection <- getDatabaseConnection
    let q = unQuery $ getQuery @(Database m) @e @a <> qWHERE " id = ?"
    Logger.sql  q
    res <- liftDatabase $ getFromDatabase @(Database m) 
        connection 
        q
        eIDs
    case res of

        [a] -> pure a
        _ -> undefined

getEntities :: forall e a m. 
    ( HasDatabase m
    , Monad m
    , Logger.HasLogger m
    , IsDatabase (Database m)
    , ToRowOf (Database m) [Page]
    , GettableFrom (Database m) e a
    , FromRowOf (Database m) (e a)
    , QConstraints (Database m)
    , Data (e a)
    , Typeable e 
    ) => 
    Page -> m [e a]
getEntities page = do
    connection <- getDatabaseConnection
    pagination <- fromString . show <$> getPaginationSize
    let q = unQuery $ getQuery @(Database m) @e @a
                   <> qLIMIT  (pagination <> "sadasdasdasd")
                   <> qOFFSET (pagination <> " * (? - 1)")
    Logger.sql q
    liftDatabase $ getFromDatabase @(Database m) 
        connection 
        q
        [page]
{-

Ambiguous type variable ‘db0’ arising from a use of ‘evalPrint’
prevents the constraint ‘(Show (QueryOf db0))’ from being solved.
Probable fix: use a type annotation to specify what ‘db0’ should be.
These potential instances exist:
  instance [safe] Show a => Show (Only a)
    -- Defined in ‘Data.Tuple.Only’
  instance [safe] (Show a, Show b) => Show (a :-> b)
    -- Defined in ‘Test.QuickCheck.Function’
  instance [safe] (Show a, Show b) => Show (Fun a b)
    -- Defined in ‘Test.QuickCheck.Function’
  ...plus 629 others
  (use -fprint-potential-instances to see them all)
-}

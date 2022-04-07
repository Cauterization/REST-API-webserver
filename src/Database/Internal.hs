module Database.Internal where

import Data.Kind (Type, Constraint)

import Database.Config

import HKD.HKD

import Logger qualified
import Extended.Text ( Text )
import Control.Exception
import App.Types
import qualified Entity.Internal as Entity
import Control.Monad.Catch
import Data.Data

class IsDatabase db where

    type QueryOf db       :: Type
    type ToRowOf db q     :: Constraint
    type FromRowOf db r   :: Constraint
    type ConnectionOf db  :: Type
    type DatabaseMonad db :: Type -> Type

    runMigrations :: Config -> Logger.Logger IO -> IO ()

    mkConnectionIO :: Config -> IO (ConnectionOf db)

    postToDatabase :: ToRowOf db e => 
        ConnectionOf db -> QueryOf db -> e -> (DatabaseMonad db) ()

    getFromDatabase :: 
        ( ToRowOf db q
        , FromRowOf db r
        ) => ConnectionOf db -> QueryOf db -> q -> (DatabaseMonad db) [r]

    putIntoDatabase :: ToRowOf db q => 
        ConnectionOf db -> QueryOf db -> q -> (DatabaseMonad db) ()

    deleteFromDatabase :: ToRowOf db [ID (Path Current)] => 
        ConnectionOf db -> QueryOf db -> [ID (Path Current)] 
            -> (DatabaseMonad db) Integer

data DBErr
    = EntityNotFound Text
    | TooManyEntities Text
    deriving (Show, Exception)

getSingle :: forall e a m. (MonadThrow m, Typeable e)=> [e a] -> m (e a)
getSingle = \case 
    [a] -> pure a
    [] -> throwM $ EntityNotFound  $ Entity.nameOf @e 
    _  -> throwM $ TooManyEntities $ Entity.nameOf @e 
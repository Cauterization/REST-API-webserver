module Database.Internal where

import Data.Kind (Type, Constraint)
import Data.String

import Database.Config


import Logger qualified
import Extended.Text ( Text )
import Extended.Text qualified as T
import Control.Exception
import App.Types
import Control.Monad.Catch
import Data.Data
import Data.List

import HKD.HKD
import Deriving.Aeson (Generic)
import qualified Database.PostgreSQL.Simple as Postgres

type QConstraints db = (IsString (QueryOf db), Monoid (QueryOf db), Show (QueryOf db))

class QConstraints db => IsDatabase db where

    type QueryOf db       :: Type
    type ToRowOf db q     :: Constraint
    type FromRowOf db r   :: Constraint
    type ConnectionOf db  :: Type
    type DatabaseMonad db :: Type -> Type

    runMigrations :: Config -> Logger.Logger IO -> IO ()

    mkConnectionIO :: Config -> IO (ConnectionOf db)

    postToDatabase :: (ToRowOf db e, FromRowOf db (ID e)) => 
        ConnectionOf db -> QueryOf db -> e -> (DatabaseMonad db) (ID e)

    getFromDatabase :: 
        ( ToRowOf db q
        , FromRowOf db r
        ) => ConnectionOf db -> QueryOf db -> q -> (DatabaseMonad db) [r]

    putIntoDatabase :: ToRowOf db q => 
        ConnectionOf db -> QueryOf db -> q -> (DatabaseMonad db) Integer

    deleteFromDatabase :: ToRowOf db [ID (e Delete)] => 
        ConnectionOf db -> QueryOf db -> [ID (e Delete)]
            -> (DatabaseMonad db) Integer

data DBError
    = EntityNotFound  Text
    | TooManyEntities Text
    | AlreadyExists   Text
    | IsNull          Text
    | OtherError      Text
    | UnknwonError    Text
    deriving (Show, Exception)

getSingle :: forall e a m. (MonadThrow m, Typeable e, Eq (e a)) => [e a] -> m (e a)
getSingle = \case 
    [a] -> pure a
    []  -> throwM $ EntityNotFound  $ nameOf @e <> " not found."
    a:as -> if all (== a) as 
            then pure a 
            else throwM $ TooManyEntities $ "Too many " <> T.pack (withPluralEnding (nameOf @e))
            
withPluralEnding :: String -> String
withPluralEnding s | "y" `isInfixOf` s = init s <> "ies"
                   | otherwise = s <> "s"

qmarks :: IsString s => Int -> s
qmarks n = fromString $ ("(" <>) $ (<> ")") $ intercalate ", " $ replicate n "?"

addWhere :: QConstraints db => QueryOf db -> QueryOf db -> QueryOf db
addWhere addition q = if "WHERE" `elem` words (show q)
                      then q <> " AND " <> addition 
                      else q <> " WHERE " <> addition


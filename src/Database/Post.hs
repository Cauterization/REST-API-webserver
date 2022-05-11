module Database.Post where

import App.Types ( fieldsOf, fieldsQuery, nameOf, ID )
import Control.Monad.Catch ( MonadThrow )
import Data.Data ( Data, Typeable )
import Data.Functor ( ($>) )
import Data.Kind ( Type )
import Data.List (intercalate)
import Data.String ( IsString(..) )
import Database.HasDatabase ( HasDatabase(..) )
import Database.Internal
    ( withPluralEnding,
      IsDatabase(QueryOf, postToDatabase, ToRowOf, FromRowOf),
      QConstraints )
import HKD.HKD ( Create )
import Logger qualified

class Postable (e :: Type -> Type) a where
  postQuery :: (IsString s, Monoid s) => s

instance {-# OVERLAPPABLE #-} (Data (e Create), Typeable e) => Postable e a where
  postQuery =
    mconcat
      [ "INSERT INTO ",
        fromString $ withPluralEnding $ nameOf @e,
        " (",
        fieldsQuery @(e Create),
        ") ",
        "VALUES ",
        qmarkfields @(e Create)
      ]

qmarkfields :: forall e s. (Data e, IsString s) => s
qmarkfields = fromString $ mconcat ["(", intercalate "," $ fieldsOf @e $> "?", ")"]

postEntity ::
  forall e (m :: Type -> Type).
  ( HasDatabase m,
    IsDatabase (Database m),
    Monad m,
    MonadThrow m,
    Logger.HasLogger m,
    Postable e Create,
    ToRowOf (Database m) (e Create),
    QConstraints (Database m),
    FromRowOf (Database m) (ID (e Create))
  ) =>
  e Create ->
  m (ID (e Create))
postEntity = postEntityWith (<> " RETURNING id")

postEntityWith ::
  forall e (m :: Type -> Type).
  ( HasDatabase m,
    IsDatabase (Database m),
    Monad m,
    MonadThrow m,
    Logger.HasLogger m,
    Postable e Create,
    ToRowOf (Database m) (e Create),
    QConstraints (Database m),
    FromRowOf (Database m) (ID (e Create))
  ) =>
  (QueryOf (Database m) -> QueryOf (Database m)) ->
  e Create ->
  m (ID (e Create))
postEntityWith f e = do
  connection <- getDatabaseConnection
  let q = postQuery @e @Create
  liftDatabase (postToDatabase @(Database m) connection (f q) e)

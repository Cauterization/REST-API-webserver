{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}

module Database.Post where

import App.Types
import Control.Monad.Catch
import Data.Data
import Data.Functor
import Data.Kind
import Data.List (intercalate)
import Data.String
import Database.HasDatabase
import Database.Internal
import HKD.HKD
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
  -- Logger.sql q
  liftDatabase (postToDatabase @(Database m) connection (f q) e)

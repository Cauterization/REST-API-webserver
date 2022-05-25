{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Database.Post where

import App.Types (ID, fieldsOf, fieldsQuery, nameOf, withPluralEnding)
import Control.Monad.Catch (MonadThrow)
import Data.Data (Data, Typeable)
import Data.Functor (($>))
import Data.Kind (Type)
import Data.List (intercalate)
import Data.String (IsString (..))
import Database.HasDatabase (HasDatabase (..))
import Database.Internal (DBQuery)
import HKD.HKD (Create)
import Logger qualified

class Postable (e :: Type -> Type) where
  postQuery :: DBQuery
  default postQuery :: (Data (e Create), Typeable e) => DBQuery
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
    Monad m,
    MonadThrow m,
    Logger.HasLogger m,
    Postable e,
    ToRowOf m (e Create),
    FromRowOf m (ID (e Create))
  ) =>
  e Create ->
  m (ID (e Create))
postEntity = postEntityWith (<> " RETURNING id")

postEntityWith ::
  forall e (m :: Type -> Type).
  ( HasDatabase m,
    Monad m,
    MonadThrow m,
    Logger.HasLogger m,
    Postable e,
    ToRowOf m (e Create),
    FromRowOf m (ID (e Create))
  ) =>
  (DBQuery -> DBQuery) ->
  e Create ->
  m (ID (e Create))
postEntityWith f e = do
  let q = postQuery @e
  postToDatabase @m (f q) e

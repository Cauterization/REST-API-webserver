{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Database.Internal where

import App.Types (nameOf, withPluralEnding)
import Control.Monad.Catch (Exception, MonadThrow (..))
import Data.Data (Typeable)
import Data.List (intercalate)
import Data.String (IsString (..))
import Extended.Text (Text)
import Extended.Text qualified as T

data DBError
  = EntityNotFound !Text
  | TooManyEntities !Text
  | AlreadyExists !Text
  | IsNull !Text
  | ConstraintViolation !Text
  | FormatError !Text
  | UnknwonError !Text
  deriving (Show, Exception)

newtype DBQuery = DBQuery {unQuery :: Text}
  deriving newtype (IsString, Show, Semigroup, Monoid)

fromQuery :: IsString q => DBQuery -> q
fromQuery = T.fromText . unQuery

getSingle :: forall e a m. (MonadThrow m, Typeable e, Eq (e a)) => [e a] -> m (e a)
getSingle = \case
  [a] -> pure a
  [] -> throwM $ EntityNotFound $ nameOf @e <> " not found."
  a : as ->
    if all (== a) as
      then pure a
      else throwM $ TooManyEntities $ "Too many " <> T.pack (withPluralEnding (nameOf @e))

qmarks :: IsString s => Int -> s
qmarks n = fromString $ ("(" <>) $ (<> ")") $ intercalate ", " $ replicate n "?"

addWhere :: DBQuery -> DBQuery -> DBQuery
addWhere addition q =
  if "WHERE" `elem` T.words (unQuery q)
    then q <> " AND " <> addition
    else q <> " WHERE " <> addition

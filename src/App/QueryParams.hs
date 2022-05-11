{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module App.QueryParams where

import Data.Char (toLower)
import Data.Function (on)
import Data.Map qualified as M
import Data.Maybe
import Data.Text.Encoding qualified as T
import Extended.Text (Text)
import Extended.Text qualified as T
import Network.HTTP.Types qualified as HTTP

type QueryParams = M.Map Text [Text]

toQueryParams :: HTTP.Query -> QueryParams
toQueryParams =
  M.mapKeys (T.map toLower) . M.unionsWith (++)
    . fmap
      ( M.map pure
          . uncurry (M.singleton `on` T.decodeUtf8)
          . fmap (fromMaybe "")
      )

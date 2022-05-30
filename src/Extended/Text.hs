{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module Extended.Text
  ( module Data.Text,
    module Data.Text.Encoding,
    module Data.Text.IO,
    read,
    show,
    fromText,
  )
where

import Control.Arrow (ArrowChoice (left))
import Data.String (IsString (..))
import Data.Text
import Data.Text.Encoding
import Data.Text.IO
import Text.Read (readEither)
import Prelude hiding (read, show)
import Prelude qualified

show :: Show a => a -> Text
show = pack . Prelude.show

read :: forall a. Read a => Text -> Either Text a
read = left pack . readEither @a . unpack

fromText :: IsString s => Text -> s
fromText = fromString . unpack

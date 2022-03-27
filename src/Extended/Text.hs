module Extended.Text
    ( module Data.Text
    , module Data.Text.Encoding
    , module Data.Text.IO
    , read
    , show
    ) where

import           Control.Arrow
import           Data.Text
import           Data.Text.Encoding
import           Data.Text.IO
import           Text.Read (readEither)

import Prelude hiding (show, read)
import Prelude qualified

show :: Show a => a -> Text
show = pack . Prelude.show

read :: forall a. Read a => Text -> Either Text a
read = left pack . readEither @a . unpack 

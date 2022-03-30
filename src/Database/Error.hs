module Database.Error where

import Control.Monad.Catch

import Data.Text (Text)

data DBErr = EntityNotFound Text
    deriving (Show, Exception)
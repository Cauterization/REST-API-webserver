module App.Result where

import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)

import App.Types
import Data.Kind (Type)
import qualified Extended.Text as T

data AppResult 
    = ResText Text 
    | ResJSON BL.ByteString
    | ResPicture
    deriving (Show, Eq)

text :: (Applicative m, Show a) => a -> m AppResult
text = pure . ResText . T.show

json :: (Applicative m, ToJSON e) => e -> m AppResult
json = pure . ResJSON . encode

type Endpoint m = forall (entity :: Type -> Type) (action :: Type). IDs -> m AppResult
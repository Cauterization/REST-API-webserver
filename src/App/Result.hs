module App.Result where

import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)

import App.Types
import Data.Kind (Type)

data AppResult 
    = ResText Text 
    | ResJSON BL.ByteString
    | ResPicture
    deriving Show 

text :: Applicative m => Text -> m AppResult
text = pure . ResText

json :: (Applicative m, ToJSON e) => e -> m AppResult
json = pure . ResJSON . encode

type Endpoint m = forall (entity :: Type -> Type) (action :: Type). IDs -> m AppResult
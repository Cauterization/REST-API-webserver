module App.Result where

import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Coerce

import App.Types
import Data.Kind (Type)
import qualified Extended.Text as T

import Entity.Picture
import Entity.Internal
import HKD.HKD

type Endpoint m = forall (entity :: Type -> Type) (action :: Type). IDs -> m AppResult

data AppResult 
    = ResText Text 
    | ResJSON BL.ByteString
    | ResPicture (Picture (Front Display))
    deriving (Show, Eq)

text :: (Applicative m, Show a) => a -> m AppResult
text = pure . ResText . T.show


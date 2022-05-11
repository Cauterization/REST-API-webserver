{-# OPTIONS_GHC -Wno-unused-foralls #-}

module App.Result where

import App.Types
import Data.ByteString.Lazy qualified as BL
import Data.Kind (Type)
import Data.Text (Text)
import Entity.Picture
import Extended.Text qualified as T
import HKD.HKD

type Endpoint m = forall (entity :: Type -> Type) (action :: Type). IDs -> m AppResult

data AppResult
  = ResText Text
  | ResJSON BL.ByteString
  | ResPicture (Picture (Front Display))
  deriving (Show, Eq)

text :: (Applicative m, Show a) => a -> m AppResult
text = pure . ResText . T.show

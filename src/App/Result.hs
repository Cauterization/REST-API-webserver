{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unused-foralls #-}

module App.Result where

import App.Path (IDs)
import Data.ByteString.Lazy qualified as BL
import Data.Kind (Type)
import Data.Text (Text)
import Entity.Picture (Picture)
import Extended.Text qualified as T
import HKD.HKD (Display, Front)

type Endpoint m = forall (entity :: Type -> Type) (action :: Type). IDs -> m AppResult

data AppResult
  = ResText !Text
  | ResJSON !BL.ByteString
  | ResPicture !(Picture (Front Display))
  deriving (Show, Eq)

text :: Applicative m => Text -> m AppResult
text = pure . ResText

toResText :: (Applicative m, Show a) => a -> m AppResult
toResText = pure . ResText . T.show

{-# LANGUAGE ImportQualifiedPost #-}

module Entity.Picture where

import Data.ByteString.Char8 qualified as BC8
import Data.ByteString.Lazy (ByteString)
import Data.Data ( Data )
import Database.Database qualified as Database
import Extended.Postgres qualified as Postgres
import Extended.Text qualified as T
import GHC.Generics ( Generic )
import HKD.HKD ( Create, Display, Front )
import Text.Read ( readMaybe )

data PictureFormat
  = JPEG
  | PNG
  | GIF
  deriving (Data, Generic, Show, Read, Eq, Ord)

instance Postgres.ToField PictureFormat where
  toField = Postgres.toField . T.show

instance Postgres.FromField PictureFormat where
  fromField f (Just mdata) = case readMaybe $ BC8.unpack mdata of
    Just JPEG -> pure JPEG
    Just PNG -> pure PNG
    Just GIF -> pure GIF
    _ -> Postgres.returnError Postgres.ConversionFailed f "Unknown picture format."
  fromField f Nothing =
    Postgres.returnError
      Postgres.UnexpectedNull
      f
      "Unexpected null in picture format."

data Picture a = Picture
  { format :: PictureFormat,
    picture :: ByteString
  }
  deriving (Data, Show, Generic, Eq, Ord, Postgres.FromRow)

instance Postgres.ToRow (Picture a) where
  toRow (Picture f p) = Postgres.toRow (f, Postgres.Binary p)

instance Database.Postable Picture Create where
  postQuery = "INSERT INTO pictures (format, picture) VALUES (?,?)"

instance Database.Gettable Picture (Front Display) where
  getQuery = "SELECT format, picture FROM pictures"

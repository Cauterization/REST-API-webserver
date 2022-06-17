{-# LANGUAGE ImportQualifiedPost #-}

module App.ResultJSON where

import App.AppT (Application)
import App.Getters (getServerAddress)
import App.Result (AppResult (ResJSON))
import App.Types (ID (ID), Token)
import Data.Aeson
  ( KeyValue ((.=)),
    ToJSON (toJSON),
    Value (Array),
    encode,
    object,
  )
import Data.Coerce (coerce)
import Data.Text (Text)
import Data.Vector qualified as Vector
import Entity.Article (Article (..))
import Entity.Author (Author )
import Entity.Category (Category)
import Entity.Draft (Draft (Draft))
import Entity.Tag (Tag)
import Entity.Internal (Entity (..))
import Entity.Picture (Picture)
import Entity.User (User)
import Extended.Text qualified as T
import HKD.HKD (Display, Front, Create)

json :: (Application m, ToJSONResult e) => e -> m AppResult
json = fmap (ResJSON . encode) . toJSONResult

-- | This class is needed to be able to pretty encode urls of pictures
-- (standart toJSON doesn't allows to get environment with server address)
class ToJSONResult a where
  toJSONResult :: Application m => a -> m Value
  default toJSONResult :: (ToJSON a, Application m) => a -> m Value
  toJSONResult = pure . toJSON

instance ToJSONResult a => ToJSONResult [a] where
  toJSONResult = fmap (Array . Vector.fromList) . mapM toJSONResult

deriving instance ToJSONResult (ID (User Create), Token)
deriving instance ToJSONResult (Entity Author (Front Display))
deriving instance ToJSONResult (Entity Category (Front Display))
deriving instance ToJSONResult (Entity Tag (Front Display))
deriving instance ToJSONResult (Entity User (Front Display))

instance ToJSONResult (Entity Draft (Front Display)) where
  toJSONResult Entity {entity = Draft a, ..} =
    toJSONResult Entity {entity = a, entityID = coerce entityID}

instance ToJSONResult (Entity Article (Front Display)) where
  toJSONResult Entity {entityID = entityID, entity = Article {..}} = do
    serverAddress <- getServerAddress
    pure $
      object
        [ "id" .= entityID,
          "title" .= title,
          "created" .= created,
          "content" .= content,
          "author" .= author,
          "category" .= category,
          "tags" .= tags,
          "pics" .= map (toPicLink serverAddress) pics
        ]

toPicLink :: Text -> ID (Picture p) -> Text
toPicLink serverAddress picID =
  T.concat
    [ serverAddress,
      "/pictures/",
      T.show picID
    ]

module App.ResultJSON where

import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Coerce
import Data.Vector qualified as Vector

import App.Types
import App.Internal
import App.Result
import Data.Kind (Type)
import qualified Extended.Text as T

import Entity.Article
import Entity.Draft
import Entity.Internal
import Entity.Picture
import HKD.HKD

json :: (Application m, ToJSONResult e) => e -> m AppResult
json = fmap (ResJSON . encode) . toJSONResult

-- | This class is needed to be able to pretty encode urls of pictures
-- (standart toJSON doesn't allows to get environment with server address)

class ToJSONResult a where
    toJSONResult :: Application m => a -> m Value

instance {-# OVERLAPPABLE #-} ToJSON a => ToJSONResult a where
    toJSONResult = pure . toJSON

instance {-# OVERLAPPABLE #-} ToJSONResult a => ToJSONResult [a] where
    toJSONResult = fmap (Array . Vector.fromList) . mapM toJSONResult

instance ToJSONResult (Entity Draft (Front Display)) where
    toJSONResult Entity{entity = Draft a, ..} 
        = toJSONResult Entity{entity = a, entityID = coerce entityID}

instance ToJSONResult (Entity Article (Front Display)) where
    toJSONResult Entity{entityID = entityID, entity = Article{..}} = do
        serverAddress <- getServerAddress
        pure $ object
            [ "id"       .= entityID
            , "title"    .= title
            , "created"  .= created
            , "content"  .= content
            , "author"   .= author
            , "category" .= category
            , "tags"     .= tags
            , "pics"     .= map (toPicLink serverAddress) pics
            ]

toPicLink :: Text -> ID (Picture p) -> Text
toPicLink serverAddress picID = T.concat
    [ serverAddress
    , "/pictures/"
    , T.show picID
    ] 
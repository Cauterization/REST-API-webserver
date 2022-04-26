{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Entity.Article where

import App.Types
import App.Internal

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Coerce
import Data.Data
import Data.Maybe   
import Database.Database qualified as Database

import Entity.Author
import Entity.Category
import Entity.Tag
import Entity.Internal
import Entity.User

import Extended.Text (Text)
import Extended.Text qualified as T

import HKD.HKD

import GHC.Generics (Generic)

import Postgres.Internal (Postgres)
import Extended.Postgres qualified as Postgres
import Data.Kind (Type)
import Data.String (IsString)

data Article a = Article
  { title    :: Field 'Required a '[NoPublish]                      Text
  , created  :: Field 'Required a '[NotAllowedFromFront, Immutable] Date 
  , content  :: Field 'Required a '[NoPublish]                      Text
  , author   :: Field 'Required a '[NotAllowedFromFront, Immutable] (EntityOrID Author a)
  , category :: Field 'Required a '[NoPublish]                      (EntityOrID Category a)
  , tags     :: Field 'Required a '[NoPublish]                      [EntityOrID Tag a]
  , pics     :: Field 'Required a '[]                               [ID Pic]
  }

data PicFormat
    deriving Data

data Pic = Pic PicFormat ByteString
    deriving Data

toPicLink :: ID a -> ID Pic -> Text
toPicLink articleID picID = T.concat
    [ serverAddress
    , "/drafts/"
    , T.show articleID
    , "/pic/"
    , T.show picID
    ] 

-- | Get / Front Display

deriving instance Data    (Article (Front Display))
deriving instance Show    (Article (Front Display))
deriving instance Generic (Article (Front Display))
instance {-# OVERLAPPING #-} ToJSON  (Entity Article (Front Display)) where
    toJSON Entity{entityID = entityID, entity = Article{..}} = object
        [ "id"       .= entityID
        , "title"    .= title
        , "created"  .= created
        , "content"  .= content
        , "author"   .= author
        , "category" .= category
        , "tags"     .= tags
        , "pics"     .= map (toPicLink entityID) pics
        ]

instance Postgres.FromRow (Article (Front Display)) where
    fromRow = do
        title     <- Postgres.field
        created   <- Postgres.field
        content   <- Postgres.field
        author    <- Postgres.fromRow
        category  <- Postgres.fromRow
        tagsNames <- catMaybes . Postgres.fromPGArray <$> Postgres.field
        tagsIDs   <- catMaybes . Postgres.fromPGArray <$> Postgres.field
        let tags = zipWith Entity tagsIDs (map Tag tagsNames)
        pics      <- map ID 
                     .  Postgres.fromPGArray 
                     .  fromMaybe (Postgres.PGArray [])
                    <$> Postgres.field --- @(Maybe (Postgres.PGArray Int))
        return Article{..}
instance Database.Gettable (Entity Article) (Front Display) where
    getQuery = articleGetQuery <> " WHERE published"

    -- getFilters = error "getFilters"

articleGetQuery :: (IsString s, Monoid s) => s
articleGetQuery = mconcat
    [ "SELECT id, title, created, content, "
    , "author_id, "
    , "user_id, ", fieldsQuery @(User (Front Display)), ", "
    , "description, "
    , "category_id[1], ", "category[1], category[2:], "
    , "tags_names, tags_id, "
    , "pics "
    , "FROM articles_view "
    ]

newtype Draft a = Draft (Article a)

deriving instance Data                          (Draft (Front Display))
deriving instance Show                          (Draft (Front Display))
deriving instance Generic                       (Draft (Front Display))
instance {-# OVERLAPPING #-} ToJSON      (Entity Draft (Front Display)) where
    toJSON Entity{entity = Draft a, ..} 
        = toJSON Entity{entity = a, entityID = coerce entityID}
instance Postgres.FromRow                       (Draft (Front Display)) where
    fromRow = Draft <$> Postgres.fromRow
instance Database.Gettable (Entity Draft) (Front Display) where
    getQuery = articleGetQuery <> " WHERE NOT published AND token = ?"
    -- type Filters (Entity Draft) (Front Display) = ArticleFilters
    -- getFilters = error "getFilters"
    
data ArticleFilters

-- instance ToJSON               (Draft  (Front Display)) where
--     toJSON (Draft d) = toJSON d
-- deriving instance Data    (Article (Front Display))
-- deriving instance Show    (Article (Front Display))
-- deriving instance Generic (Article (Front Display))
-- deriving instance ToJSON  (Article (Front Display))
-- instance Postgres.FromRow (Article (Front Display)) where
--     fromRow = do
--         

-- instance Database.GettableFrom Postgres Article (Front Display) where
--     getQuery = mconcat
--         [ "SELECT title, created, content, "
--         , fieldsQuery @(User (Front Display)), ", "
--         , "description, "
--         , "category[1], category[2:], "
--         , "tags_names "
--         , "FROM articles_view "
--         ]

-- deriving instance Generic        (Article (Front Create))
-- deriving instance FromJSON       (Article (Front Create))
-- deriving instance Generic        (Article Create)
-- deriving instance Data           (Article Create)
-- deriving instance Postgres.ToRow (Article Create) 
-- instance Database.PostableTo Postgres Article where
--     postQuery = mconcat
--         [ "WITH isertArticle AS "
--         , "( INSERT INTO Articles (title, created, content, author, category, published) "
--         , "VALUES (?,?,?,?,?, false) "
--         , "RETURNING id AS article_id), "
--         , "insertTags AS (INSERT INTO article_tag (article_id, tag_id) "
--         , "SELECT article_id, tag_id "
--         , "FROM isertArticle CROSS JOIN UNNEST(?) as tag_id "
--         , "RETURNING article_id "
--         , ") SELECT * FROM insertTags LIMIT 1"
--         ]

-- instance Postgres.ToRow (Entity Article Publish) where
--     toRow (Entity eID Article{..}) = 
--         [ Postgres.toField created
--         , Postgres.toField eID
--         , Postgres.toField author
--         ]

-- publishQuery :: (IsString s, Monoid s) => s 
-- publishQuery = mconcat
--     [ "UPDATE articles "
--     , "SET published = true, "
--     , "created = ? "
--     , "WHERE id = ? AND published = false "
--     , "AND author = ? "
--     , "RETURNING id"
--     ]

-- deriving instance Generic  (Article (Front Update))
-- deriving instance FromJSON (Article (Front Update))
-- deriving instance Data     (Article Update)

-- instance Database.ToOneRow (Article (Front Update)) IDs where

--     type instance MkOneRow (Article (Front Update)) IDs 
--         = ( Maybe Text
--           , Maybe NotUpdated
--           , Maybe Text
--           , Maybe NotUpdated
--           , Maybe (ID (Category (Front Update)))
--           , Maybe [ID (Tag (Front Update))]
--           , ID (Path Current)) 

--     toOneRow Article{..} [aID] = pure 
--         (title, Nothing, content, Nothing, category, tags, aID)
--     toOneRow _ _ = entityIDArityMissmatch "update article"


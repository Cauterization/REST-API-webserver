{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Entity.Draft where

import App.Types
import App.Internal

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Coerce
import Data.Data

import Data.Maybe   
import Data.List
import Database.Database qualified as Database

import Entity.Author
import Entity.Article
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
import Data.String (IsString(..))

newtype Draft a = Draft {unDraft :: Article a}

-- | Post

deriving instance Generic          (Draft (Front Create))
deriving newtype instance FromJSON (Draft (Front Create))
deriving instance Show (Draft Create)
deriving instance Data (Draft Create)

instance Postgres.ToRow (Draft Create) where
    toRow (Draft Article{..}) 
        = Postgres.toRow (title, created, content, author, category, tags, title)

instance Database.Postable Draft Create where
    postQuery = mconcat
        [ "WITH insertion AS "
        , "(  INSERT INTO articles "
        , "   (title, created, content, author, category, published) "
        , "   VALUES(?,?,?,(SELECT id FROM authors WHERE user_id = ?),?,false) "
        , "   RETURNING ID "
        , ") "
        , "INSERT INTO article_tag (article_id, tag_id) "
        , "(SELECT id, UNNEST(?::INTEGER[]) FROM INSERTION); "
        , "SELECT id FROM articles WHERE title = ? "
        ]

-- | Get
deriving instance Eq                            (Draft (Front Display))
deriving instance Data                          (Draft (Front Display))
deriving instance Show                          (Draft (Front Display))
deriving instance Generic                       (Draft (Front Display))
instance {-# OVERLAPPING #-} ToJSON      (Entity Draft (Front Display)) where
    toJSON Entity{entity = Draft a, ..} 
        = toJSON Entity{entity = a, entityID = coerce entityID}
instance Postgres.FromRow                       (Draft (Front Display)) where
    fromRow = Draft <$> Postgres.fromRow
instance Database.Gettable (Entity Draft) (Front Display) where
    getQuery = articleGetQuery <> " WHERE NOT published "

-- | Put
deriving instance Generic  (Draft (Front Update))
deriving instance Data     (Draft (Front Update))
deriving newtype instance FromJSON (Draft (Front Update))
type DraftPutArgs = (Draft (Front Update), Token, ID (Draft (Front Update)))
instance Postgres.ToRow (Entity Draft (Front Update)) where
    toRow Entity{entityID = eID, entity = Draft Article{..}}
        = Postgres.toRow (title, content, category, eID, tags, eID) 

instance Database.Puttable (Entity Draft (Front Update)) where
    putQuery = mconcat
        [ "UPDATE articles "
        , "SET "
        , Database.toCoalesce ["title", "content", "category"]
        , " WHERE id = ?"
        , "; "
        , "WITH new_tags AS (VALUES (? :: INT []))"
        , ", tags_update AS "
        , "(DELETE FROM article_tag WHERE id = ? AND tag_id NOT IN (SELECT * FROM new_tags)) "
        , "INSERT INTO article_tag VALUES "
        , "IF IS NULL new_tags "
        , "THEN () "
        , "ELSE (SELECT * FROM tags_update) "
        ]

UPDATE articles 
SET title = COALESCE (null, title)
, content = COALESCE (null, content)
, category = COALESCE (null, category) 
WHERE id = 45; 

WITH 
    new_tags (tags) AS (VALUES (null :: INT [])),
    tags_update AS 
        ( DELETE FROM article_tag 
          WHERE id = 45 AND tag_id NOT IN (SELECT * FROM new_tags)
        ) 
INSERT INTO article_tag 
VALUES (IF (SELECT tags FROM new_tags) IS NULL
       THEN SELECT * FROM tags_update END IF);

-- UPDATE articles 
-- SET title = COALESCE (null, title)
--   , content = COALESCE (null, content)
--   , category = COALESCE (null, category)
-- WHERE id = 45;

-- WITH new_tags AS (VALUES (? :: INT []))
-- , tags_update AS 
--     (DELETE FROM article_tag WHERE id = 45 AND tag_id NOT IN (SELECT * FROM new_tags); 
--      SELECT * FROM new_tags) 
-- INSERT INTO article_tag VALUES IF new_tags IS NULL THEN () ELSE (SELECT * FROM tags_update);

--     data Article a = Article
--   { title    :: Field 'Required a '[NoPublish]                      Text
--   , created  :: Field 'Required a '[NotAllowedFromFront, Immutable] Date 
--   , content  :: Field 'Required a '[NoPublish]                      Text
--   , author   :: Field 'Required a '[NotAllowedFromFront, Immutable] (EntityOrID Author a)
--   , category :: Field 'Required a '[NoPublish]                      (EntityOrID Category a)
--   , tags     :: Field 'Required a '[NoPublish]                      [EntityOrID Tag a]
--   , pics     :: Field 'Required a '[]                               [ID Pic]
--   }

    -- putQuery = mconcat
    --     [ "UPDATE " , fromString $ withPluralEnding $ nameOf @e
    --     , " SET "
    --     , fromString $ intercalate ", " $ map fieldToCoalesce  
    --         $ fieldsOf @(e a)
    --     , " WHERE id = ? "
    --     ]

-- instance {-# OVERLAPPING #-} Postgres.ToRow DraftPutArgs where
--     toRow (Draft Article{..}, token, draftID) = toRow 
--         ( title, created, content, author, category, token, draftID
        
        -- )
-- instance Database.Puttable DraftPutArgs where
--     putQuery = mconcat
--         [ "UPDATE articles SET"
--         , 
--         ]




--       putQuery = mconcat
--         [ "UPDATE " , fromString $ withPluralEnding $ nameOf @e
--         , " SET "
--         , fromString $ intercalate ", " $ map fieldToCoalesce  
--             $ fieldsOf @(e a)
--         , " WHERE id = ? "
--         ]

-- fieldToCoalesce :: (Semigroup a, IsString a) => a -> a
-- fieldToCoalesce str =  str <> " = COALESCE (?, " <> str <> ")"
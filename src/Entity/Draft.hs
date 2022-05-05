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

deriving instance (Data a, Data (Article a)) => Data (Draft a)
deriving instance         (Show (Article a)) => Show (Draft a)


-- | Post

deriving newtype instance FromJSON                    (Draft (Front Create))
deriving via (Article Create) instance Postgres.ToRow (Draft Create) 
instance Database.Postable                             Draft Create where
    postQuery = "SELECT post_draft " <> Database.qmarkFields @(Article Create)

-- | Get

deriving instance Eq                                  (Draft (Front Display))

instance Postgres.FromRow                             (Draft (Front Display)) where
    fromRow = Draft <$> Postgres.fromRow
instance Database.Gettable                    (Entity Draft) (Front Display) where
    getQuery = articleGetQuery <> " WHERE NOT published "

-- | Put

deriving newtype instance FromJSON (Draft (Front Update))

instance Postgres.ToRow (Entity Draft (Front Update)) where
    toRow (Entity draftID (Draft Article{..})) 
        = Postgres.toRow (title, content, category, tags, pics, draftID, draftID) 

-- | Last query here preventing entity not found error due to procedure execution
-- (execute with procedure dosn't returns number of affected rows)
instance Database.Puttable (Entity Draft (Front Update)) where
    putQuery = mconcat
        [ "CALL put_draft"
        , "(?,?,?,?::INTEGER [], ?::INTEGER [], ?); "
        , "UPDATE articles SET title = title WHERE id = ?"
        ]

-- | Delete
instance Database.Deletable Draft Delete where
    deleteQuery = Database.deleteQuery @Article @Delete

-- | Publish
deriving newtype instance EmptyData (Draft Publish) 

instance Postgres.ToRow (Entity Draft Publish) where
    toRow Entity{..} = Postgres.toRow entityID

instance Database.Puttable (Entity Draft Publish) where
    putQuery = "UPDATE articles SET published = true WHERE id = ?"


-- deriving via (Entity Article) Delete instance Database.Deletable (Entity Draft) Delete

-- instance Database.Deletable Draft Delete where
--     deleteQuery = "ASD"


{-
>>> Database.deleteQuery @Draft @Delete
-}

{-
>>> Database.putQuery @(Entity Article (Front Update))
-}
-- UPDATE articles 
-- SET title = COALESCE (null, title)
-- , content = COALESCE (null, content)
-- , category = COALESCE (null, category) 
-- WHERE id = 45; 

-- WITH new_tags AS (VALUES (null :: INT []))

-- WITH 
--     new_tags (tags) AS (VALUES (null :: INT [])),
--     tags_update AS 
--         ( DELETE FROM article_tag 
--           WHERE id = 45 AND tag_id NOT IN (SELECT * FROM new_tags)
--         ) 
-- INSERT INTO article_tag 
-- VALUES (IF (SELECT tags FROM new_tags) IS NULL
--        THEN SELECT * FROM tags_update END IF);

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



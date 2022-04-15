{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Entity.Article where

import App.Types

import Data.Aeson
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
  { title    :: Field "title"     'Required a '[NoPublish] Text
  , created  :: Field "created"   'Required a '[NotAllowedFromFront] Date 
  , content  :: Field "content"   'Required a '[NoPublish] Text
  , author   :: Field "author"    'Required a '[NotAllowedFromFront] (EntityOrID Author a)
  , category :: Field "category"  'Required a '[NoPublish] (EntityOrID Category a)
  , tags     :: Field "tags"      'Required a '[NoPublish] [EntityOrID Tag a]
  }

deriving instance Data    (Article (Front Display))
deriving instance Show    (Article (Front Display))
deriving instance Generic (Article (Front Display))
deriving instance ToJSON  (Article (Front Display))
instance Postgres.FromRow (Article (Front Display)) where
    fromRow = do
        title    <- Postgres.field
        created  <- Postgres.field
        content  <- Postgres.field
        author   <- Postgres.fromRow
        category <- Postgres.fromRow
        tags     <- catMaybes . Postgres.fromPGArray <$> Postgres.field
        return Article{..}

instance Database.GettableFrom Postgres Article (Front Display) where
    getQuery = mconcat
        [ "SELECT title, created, content, "
        , fieldsQuery @(User (Front Display)), ", "
        , "description, "
        , "category[1], category[2:], "
        , "tags_names "
        , "FROM articles_view "
        ]

deriving instance Generic        (Article (Front Create))
deriving instance FromJSON       (Article (Front Create))
deriving instance Generic        (Article Create)
deriving instance Data           (Article Create)
deriving instance Postgres.ToRow (Article Create) 
instance Database.PostableTo Postgres Article where
    postQuery = mconcat
        [ "WITH isertArticle AS "
        , "( INSERT INTO Articles (title, created, content, author, category, published) "
        , "VALUES (?,?,?,?,?, false) "
        , "RETURNING id AS article_id), "
        , "insertTags AS (INSERT INTO article_tag (article_id, tag_id) "
        , "SELECT article_id, tag_id "
        , "FROM isertArticle CROSS JOIN UNNEST(?) as tag_id "
        , "RETURNING article_id "
        , ") SELECT * FROM insertTags LIMIT 1"
        ]

instance Postgres.ToRow (Entity Article Publish) where
    toRow (Entity eID Article{..}) = 
        [ Postgres.toField created
        , Postgres.toField eID
        , Postgres.toField author
        ]

publishQuery :: (IsString s, Monoid s) => s 
publishQuery = mconcat
    [ "UPDATE articles "
    , "SET published = true, "
    , "created = ? "
    , "WHERE id = ? AND published = false "
    , "AND author = ? "
    , "RETURNING id"
    ]
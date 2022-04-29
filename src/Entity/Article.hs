{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Entity.Article where

import App.Types
import App.Internal

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
import Entity.Picture

import Extended.Text (Text)
import Extended.Text qualified as T

import HKD.HKD

import GHC.Generics (Generic)

import Postgres.Internal (Postgres)
import Extended.Postgres qualified as Postgres
import Data.Kind (Type)
import Data.String (IsString)

data Article a = Article
  { title    :: Field 'Required a '[]                               Text
  , created  :: Field 'Required a '[NotAllowedFromFront, Immutable] Date 
  , content  :: Field 'Required a '[]                               Text
  , author   :: Field 'Required a '[NotAllowedFromFront, Immutable] (EntityOrID Author a)
  , category :: Field 'Required a '[]                               (EntityOrID Category a)
  , tags     :: Field 'Required a '[]                               [EntityOrID Tag a]
  , pics     :: Field 'Required a '[]                               [ID (Picture a)]
  }

deriving instance Generic (Article a)

deriving instance 
    ( Data a
    , Data (Field 'Required a '[]                               Text)
    , Data (Field 'Required a '[NotAllowedFromFront, Immutable] Date)
    , Data (Field 'Required a '[]                               Text)
    , Data (Field 'Required a '[NotAllowedFromFront, Immutable] (EntityOrID Author a))
    , Data (Field 'Required a '[]                               (EntityOrID Category a))
    , Data (Field 'Required a '[]                               [EntityOrID Tag a])
    , Data (Field 'Required a '[] [ID (Picture a)])
    ) => Data (Article a)

-- | Post
deriving instance FromJSON       (Article (Front Create))
deriving instance Show           (Article Create)
deriving instance Postgres.ToRow (Article Create)

-- | Get / Front Display
deriving instance Eq      (Article (Front Display))
deriving instance Show    (Article (Front Display))

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
        pics      <- map ID . catMaybes . Postgres.fromPGArray <$> Postgres.field 
        pure Article{..}
        
instance Database.Gettable (Entity Article) (Front Display) where

    getQuery = articleGetQuery <> "WHERE published"

    entityFilters = (<> Database.defaultFilters)
        [ Database.EFDate    "crAt"
        , Database.EFDate    "crAtLt"
        , Database.EFDate    "crAtGt"
        , Database.EFString  "author_login"
        , Database.EFNum     "category_id"
        , Database.EFNum     "tag_id"
        , Database.EFNumList "tag_in"
        , Database.EFNumList "tag_all"
        , Database.EFString  "title"
        , Database.EFString  "content"
        , Database.EFString  "substring"
        ]

    entityFiltersQuery = ""

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

articlesGetQuery :: (IsString s, Monoid s) => s -> s
articlesGetQuery ordering = mconcat
    [ "WITH F (crAt, crAtLt, crAtGt, authorF, catF, tagF, tagsIn, tagsAll, "
    , "titleF, contentF, substring) AS (VALUES"
    , "( ? :: DATE "
    , ", ? :: DATE "
    , ", ? :: DATE "
    , ", ? :: TEXT "
    , ", ? :: INT "
    , ", ? :: INT "
    , ", ? :: INT [] "
    , ", ? :: INT [] "
    , ", ? :: TEXT "
    , ", ? :: TEXT "
    , ", ? :: TEXT"
    , ")) "
    , articleGetQuery
    , ", F "
    , "WHERE published "
    , "  AND created     =  COALESCE(created, crAt) "
    , "  AND created     >= COALESCE(created, crAtGt) " 
    , "  AND created     <= COALESCE(created, crAtLt) " 
    , "  AND login       =  COALESCE(login, authorF) "  
    , "  AND (catF       IS NULL OR category_id @> ARRAY [catF]) " 
    , "  AND (tagF       IS NULL OR tags_id @> ARRAY [tagF]) "  
    , "  AND (tagsIn     IS NULL OR tags_id && tagsIn) "  
    , "  AND (tagsAll    IS NULL OR tags_id @> tagsAll) "   
    , "  AND title       LIKE CONCAT('%',COALESCE(title, titleF),'%') "  
    , "  AND content     LIKE CONCAT('%',COALESCE(content, contentF),'%') "      
    , "  AND (substring  IS NULL "    
    , "      OR content  LIKE CONCAT('%',substring,'%') "   
    , "      OR login    LIKE CONCAT('%',substring,'%') "    
    , "      OR EXISTS ( SELECT 1 "    
    , "                  FROM UNNEST(category) "   
    , "                  WHERE unnest LIKE CONCAT('%',substring,'%') "   
    , "                ) "   
    , "      OR EXISTS ( SELECT 1 "    
    , "                  FROM UNNEST(tags_names) "  
    , "                  WHERE unnest LIKE CONCAT('%',substring,'%') "    
    , "      )         ) " 
    , ordering   
    , " LIMIT ? "
    , "OFFSET ? "
    ] 


-- | Put
deriving instance FromJSON       (Article (Front Update))
deriving instance Postgres.ToRow (Article (Front Update))

-- | Other
deriving instance EmptyData (Article Publish) 
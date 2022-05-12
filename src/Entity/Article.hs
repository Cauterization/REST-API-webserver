{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Entity.Article where

import App.Types ( fieldsQuery, Date, ID(ID) )
import Data.Aeson ( FromJSON )
import Data.Data ( Data )
import Data.Maybe ( catMaybes )
import Data.String (IsString)
import Database.Database qualified as Database
import Entity.Author ( Author )
import Entity.Category ( Category )
import Entity.Internal ( EntityOrID, Entity(Entity) )
import Entity.Picture ( Picture )
import Entity.Tag ( Tag(Tag) )
import Entity.User ( User )
import Extended.Postgres qualified as Postgres
import Extended.Text (Text)
import GHC.Generics (Generic)
import HKD.HKD
    ( EmptyData,
      Field,
      Create,
      Display,
      Immutable,
      Update,
      Publish,
      Front,
      NotAllowedFromFront )

data Article a = Article
  { title :: !(Field a '[] Text),
    created :: !(Field a '[NotAllowedFromFront, Immutable] Date),
    content :: !(Field a '[] Text),
    author :: !(Field a '[NotAllowedFromFront, Immutable] (EntityOrID Author a)),
    category :: !(Field a '[] (EntityOrID Category a)),
    tags :: !(Field a '[] [EntityOrID Tag a]),
    pics :: !(Field a '[] [ID (Picture a)])
  }
  deriving stock (Generic)

deriving instance
  ( Data a,
    Data (Field a '[] Text),
    Data (Field a '[NotAllowedFromFront, Immutable] Date),
    Data (Field a '[] Text),
    Data (Field a '[NotAllowedFromFront, Immutable] (EntityOrID Author a)),
    Data (Field a '[] (EntityOrID Category a)),
    Data (Field a '[] [EntityOrID Tag a]),
    Data (Field a '[] [ID (Picture a)])
  ) =>
  Data (Article a)

deriving instance
  ( Show (Field a '[] Text),
    Show (Field a '[NotAllowedFromFront, Immutable] Date),
    Show (Field a '[] Text),
    Show (Field a '[NotAllowedFromFront, Immutable] (EntityOrID Author a)),
    Show (Field a '[] (EntityOrID Category a)),
    Show (Field a '[] [EntityOrID Tag a]),
    Show (Field a '[] [ID (Picture a)])
  ) =>
  Show (Article a)

deriving instance
  ( Eq (Field a '[] Text),
    Eq (Field a '[NotAllowedFromFront, Immutable] Date),
    Eq (Field a '[] Text),
    Eq (Field a '[NotAllowedFromFront, Immutable] (EntityOrID Author a)),
    Eq (Field a '[] (EntityOrID Category a)),
    Eq (Field a '[] [EntityOrID Tag a]),
    Eq (Field a '[] [ID (Picture a)])
  ) =>
  Eq (Article a)

-- | Post
deriving instance FromJSON (Article (Front Create))

deriving instance Postgres.ToRow (Article Create)

-- | Get / Front Display
instance Postgres.FromRow (Article (Front Display)) where
  fromRow = do
    title <- Postgres.field
    created <- Postgres.field
    content <- Postgres.field
    author <- Postgres.fromRow
    category <- Postgres.fromRow
    tagsNames <- catMaybes . Postgres.fromPGArray <$> Postgres.field
    tagsIDs <- catMaybes . Postgres.fromPGArray <$> Postgres.field
    let tags = zipWith Entity tagsIDs (map Tag tagsNames)
    pics <- map ID . catMaybes . Postgres.fromPGArray <$> Postgres.field
    pure Article {..}

instance Database.Gettable (Entity Article) (Front Display) where
  getQuery = articleGetQuery <> "WHERE published"

  entityFilters =
    (<> Database.defaultFilters)
      [ Database.EFDate "crAt",
        Database.EFDate "crAtLt",
        Database.EFDate "crAtGt",
        Database.EFString "author_login",
        Database.EFNum "category_id",
        Database.EFNum "tag_id",
        Database.EFNumList "tag_in",
        Database.EFNumList "tag_all",
        Database.EFString "title",
        Database.EFString "content",
        Database.EFString "substring"
      ]

  entityFiltersQuery = ""

articleGetQuery :: (IsString s, Monoid s) => s
articleGetQuery =
  mconcat
    [ "SELECT id, title, created, content, ",
      "author_id, ",
      "user_id, ",
      fieldsQuery @(User (Front Display)),
      ", ",
      "description, ",
      "category_id[1], ",
      "category[1], category[2:], ",
      "tags_names, tags_id, ",
      "pics ",
      "FROM articles_view "
    ]

articlesGetQuery :: (IsString s, Monoid s) => s -> s
articlesGetQuery ordering =
  mconcat
    [ "WITH F (crAt, crAtLt, crAtGt, authorF, catF, tagF, tagsIn, tagsAll, ",
      "titleF, contentF, substring) AS (VALUES",
      "( ? :: DATE ",
      ", ? :: DATE ",
      ", ? :: DATE ",
      ", ? :: TEXT ",
      ", ? :: INT ",
      ", ? :: INT ",
      ", ? :: INT [] ",
      ", ? :: INT [] ",
      ", ? :: TEXT ",
      ", ? :: TEXT ",
      ", ? :: TEXT",
      ")) ",
      articleGetQuery,
      ", F ",
      "WHERE published ",
      "  AND created     =  COALESCE(created, crAt) ",
      "  AND created     >= COALESCE(created, crAtGt) ",
      "  AND created     <= COALESCE(created, crAtLt) ",
      "  AND login       =  COALESCE(login, authorF) ",
      "  AND (catF       IS NULL OR category_id @> ARRAY [catF]) ",
      "  AND (tagF       IS NULL OR tags_id @> ARRAY [tagF]) ",
      "  AND (tagsIn     IS NULL OR tags_id && tagsIn) ",
      "  AND (tagsAll    IS NULL OR tags_id @> tagsAll) ",
      "  AND title       LIKE CONCAT('%',COALESCE(title, titleF),'%') ",
      "  AND content     LIKE CONCAT('%',COALESCE(content, contentF),'%') ",
      "  AND (substring  IS NULL ",
      "      OR content  LIKE CONCAT('%',substring,'%') ",
      "      OR login    LIKE CONCAT('%',substring,'%') ",
      "      OR EXISTS ( SELECT 1 ",
      "                  FROM UNNEST(category) ",
      "                  WHERE unnest LIKE CONCAT('%',substring,'%') ",
      "                ) ",
      "      OR EXISTS ( SELECT 1 ",
      "                  FROM UNNEST(tags_names) ",
      "                  WHERE unnest LIKE CONCAT('%',substring,'%') ",
      "      )         ) ",
      ordering,
      " LIMIT ? ",
      "OFFSET ? "
    ]

-- | Put
deriving instance FromJSON (Article (Front Update))

deriving instance Postgres.ToRow (Article (Front Update))

-- | Other
deriving instance EmptyData (Article Publish)

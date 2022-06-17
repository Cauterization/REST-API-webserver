{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Entity.Article where

import App.Types (Date, ID (ID), fieldsQuery)
import Data.Aeson (FromJSON)
import Data.Char (toLower)
import Data.Data (Data)
import Data.Kind (Type, Constraint)
import Data.Maybe (catMaybes)
import Data.String (IsString (fromString))
import Database.EntityFilters qualified as Database
import Database.Get qualified as Database
import Database.Post qualified as Database
import Database.Put qualified as Database
import Entity.Author (Author)
import Entity.Category (Category)
import Entity.Internal (Entity (Entity), EntityOrID)
import Entity.Picture (Picture)
import Entity.Tag (Tag (Tag))
import Entity.User (User)
import Extended.Postgres qualified as Postgres
import Extended.Text (Text)
import Extended.Text qualified as T
import GHC.Generics (Generic)
import HKD.HKD
  ( Create,
    Display,
    EmptyData,
    Field,
    Front,
    Immutable,
    NotAllowedFromFront,
    Publish,
    Update,
  )

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

type ArticleFieldsConstraint a (constr :: Type -> Constraint) =
  ( constr (Field a '[] Text),
    constr (Field a '[NotAllowedFromFront, Immutable] Date),
    constr (Field a '[] Text),
    constr (Field a '[NotAllowedFromFront, Immutable] (EntityOrID Author a)),
    constr (Field a '[] (EntityOrID Category a)),
    constr (Field a '[] [EntityOrID Tag a]),
    constr (Field a '[] [ID (Picture a)])
  )

deriving instance
  ( Data a,
    ArticleFieldsConstraint a Data
  ) =>
  Data (Article a)

deriving instance
  ( ArticleFieldsConstraint a Show
  ) =>
  Show (Article a)

deriving instance
  ( ArticleFieldsConstraint a Eq
  ) =>
  Eq (Article a)

-- | Post
deriving instance FromJSON (Article (Front Create))

deriving instance Postgres.ToRow (Article Create)

instance Database.Postable Article

-- | Get / Front Display
data OrderDirection = AscOD | DescOD

data OrderField = DateOF | AuthorOF | CategoryOF | PhotosOF | IDOF

data ArticleOrder = ArticleOrder OrderField OrderDirection

defaultOrderField :: OrderField
defaultOrderField = IDOF

defaultOrder :: ArticleOrder
defaultOrder = ArticleOrder defaultOrderField AscOD

parseOrder :: Text -> ArticleOrder
parseOrder (T.break (== ',') -> (field, direction)) = ArticleOrder fieldOrder directionOrder
  where
    fieldOrder = case T.map toLower field of
      "date" -> DateOF
      "author" -> AuthorOF
      "category" -> CategoryOF
      "photos" -> PhotosOF
      _ -> defaultOrderField
    directionOrder = case T.map toLower direction of
      ",desc" -> DescOD
      _ -> AscOD

orderToQuery :: IsString s => ArticleOrder -> s
orderToQuery (ArticleOrder field direction) =
  fromString $ "ORDER BY " <> fieldOrder <> " " <> directionOrder
  where
    fieldOrder = case field of
      DateOF -> "created"
      AuthorOF -> "login"
      CategoryOF -> "category[1]"
      PhotosOF -> "array_length(pics,1)"
      IDOF -> "id"
    directionOrder = case direction of
      DescOD -> "DESC"
      _ -> ""

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

articlesGetQuery :: (IsString s, Monoid s) => ArticleOrder -> s
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
      "  AND created     =  COALESCE(crAt, created) ",
      "  AND created     >= COALESCE(crAtGt, created) ",
      "  AND created     <= COALESCE(crAtLt, created) ",
      "  AND login       =  COALESCE(authorF, login) ",
      "  AND (catF       IS NULL OR category_id @> ARRAY [catF]) ",
      "  AND (tagF       IS NULL OR tags_id @> ARRAY [tagF]) ",
      "  AND (tagsIn     IS NULL OR tags_id && tagsIn) ",
      "  AND (tagsAll    IS NULL OR tags_id @> tagsAll) ",
      "  AND title       LIKE CONCAT('%',COALESCE(titleF, title),'%') ",
      "  AND content     LIKE CONCAT('%',COALESCE(contentF, content),'%') ",
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
      orderToQuery ordering,
      " LIMIT ? ",
      "OFFSET ? "
    ]

-- | Put
deriving instance FromJSON (Article (Front Update))

deriving instance Postgres.ToRow (Article (Front Update))

instance Database.Puttable (Article (Front Update))

-- | Other
deriving instance EmptyData (Article Publish)

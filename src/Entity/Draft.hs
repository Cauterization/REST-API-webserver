{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Entity.Draft where

import Data.Aeson ( FromJSON )
import Data.Data ( Data )
import Database.Database qualified as Database
import Entity.Article ( articleGetQuery, Article(..) )
import Entity.Internal ( Entity(..) )
import Extended.Postgres qualified as Postgres
import GHC.Generics (Generic)
import HKD.HKD
    ( EmptyData, Create, Delete, Display, Update, Publish, Front )

newtype Draft a = Draft {unDraft :: Article a} deriving stock (Generic)

deriving instance (Data a, Data (Article a)) => Data (Draft a)

deriving instance (Show (Article a)) => Show (Draft a)

deriving instance Eq (Article a) => Eq (Draft a)

-- | Post
deriving newtype instance FromJSON (Draft (Front Create))

deriving via (Article Create) instance Postgres.ToRow (Draft Create)

instance Database.Postable Draft Create where
  postQuery =
    mconcat
      [ "SELECT post_draft",
        "( ? :: TEXT       ",
        ", ? :: DATE       ",
        ", ? :: TEXT       ",
        ", ? :: INTEGER    ",
        ", ? :: INTEGER    ",
        ", ? :: INTEGER [] ",
        ", ? :: INTEGER [] ",
        ")"
      ]

-- | Get
instance Postgres.FromRow (Draft (Front Display)) where
  fromRow = Draft <$> Postgres.fromRow

instance Database.Gettable (Entity Draft) (Front Display) where
  getQuery = articleGetQuery <> " WHERE NOT published "

-- | Put
deriving newtype instance FromJSON (Draft (Front Update))

instance Postgres.ToRow (Entity Draft (Front Update)) where
  toRow (Entity draftID (Draft Article {..})) =
    Postgres.toRow (title, content, category, tags, pics, draftID, draftID)

-- | Last query here preventing entity not found error due to procedure execution
-- (execute with procedure dosn't returns number of affected rows)
instance Database.Puttable (Entity Draft (Front Update)) where
  putQuery =
    mconcat
      [ "CALL put_draft",
        "(?,?,?,?::INTEGER [], ?::INTEGER [], ?); ",
        "UPDATE articles SET title = title WHERE id = ?"
      ]

-- | Delete
instance Database.Deletable Draft Delete where
  deleteQuery = Database.deleteQuery @Article @Delete

-- | Publish
deriving newtype instance EmptyData (Draft Publish)

instance Postgres.ToRow (Entity Draft Publish) where
  toRow Entity {..} = Postgres.toRow entityID

instance Database.Puttable (Entity Draft Publish) where
  putQuery = "UPDATE articles SET published = true WHERE id = ?"

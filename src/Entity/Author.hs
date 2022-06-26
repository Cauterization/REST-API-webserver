module Entity.Author where

import App.Types (fieldsQuery)
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.Generics.Product.Fields qualified as GL
import Data.Text (Text)
import Database.Delete qualified as Database
import Database.Get qualified as Database
import Database.Post qualified as Database
import Database.Put qualified as Database
import Entity.Internal (Entity (..), EntityOrID, entityFromRow)
import Entity.User (User)
import Extended.Postgres qualified as Postgres
import GHC.Generics (Generic)
import HKD.HKD (Create, Display, Field, Front, Immutable, Update)

data Author a = Author
  { user :: !(Field a '[Immutable] (EntityOrID User a)),
    description :: !(Field a '[] Text)
  }
  deriving stock (Generic)

instance
  {-# OVERLAPPING #-}
  (GL.HasField' name (Author f) a, f ~ g, a ~ b) =>
  GL.HasField name (Author f) (Author g) a b
  where
  field = GL.field' @name

deriving instance
  ( Data a,
    Data (Field a '[Immutable] (EntityOrID User a)),
    Data (Field a '[] Text)
  ) =>
  Data (Author a)

deriving instance
  ( Show (Field a '[Immutable] (EntityOrID User a)),
    Show (Field a '[] Text)
  ) =>
  Show (Author a)

deriving instance
  ( Eq (Field a '[Immutable] (EntityOrID User a)),
    Eq (Field a '[] Text)
  ) =>
  Eq (Author a)

-- | Post
deriving instance FromJSON (Author (Front Create))

deriving instance Postgres.ToRow (Author Create)

instance Database.Postable Author where
  postQuery = "INSERT INTO authors (user_id, description) VALUES (?,?)"

-- | Get
deriving instance ToJSON (Author (Front Display))

instance Postgres.FromRow (Author (Front Display)) where
  fromRow = do
    user <- Postgres.fromRow
    description <- Postgres.field
    pure Author {..}

instance Postgres.FromRow (Entity Author (Front Display)) where
  fromRow = entityFromRow

instance Database.Gettable (Entity Author) (Front Display) where
  getQuery =
    mconcat
      [ "SELECT ",
        "id, ",
        "user_id, ",
        fieldsQuery @(User (Front Display)),
        ", description",
        " FROM authors_view"
      ]

-- | Put
deriving instance FromJSON (Author (Front Update))

deriving instance Postgres.ToRow (Author (Front Update))

instance Postgres.ToRow (Entity Author (Front Update)) where
  toRow Entity {..} = Postgres.toRow entity ++ Postgres.toRow entityID

instance Database.Puttable (Author (Front Update)) where
  putQuery =
    mconcat
      [ "UPDATE authors ",
        "SET user_id = COALESCE (?, user_id), ",
        "description = COALESCE (?, description) ",
        "WHERE id = ? "
      ]

-- | Delete
instance Database.Deletable Author

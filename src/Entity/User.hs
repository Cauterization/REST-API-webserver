{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Entity.User where

import App.Types (Date)
import Data.Aeson (FromJSON (..), ToJSON (..), camelTo2, defaultOptions, fieldLabelModifier, genericParseJSON, genericToJSON, omitNothingFields)
import Data.Aeson qualified as A
import Data.Data (Data)
import Data.Generics.Product.Fields qualified as GL
import Data.Text (Text)
import Database.Delete qualified as Database
import Database.Get qualified as Database
import Database.Post qualified as Database
import Database.PostgreSQL.Simple qualified as Postgres
import Database.Put qualified as Database
import Entity.Internal (Entity)
import GHC.Generics (Generic)
import HKD.HKD
  ( Contains,
    Create,
    Display,
    EmptyData,
    Field,
    Front,
    Hidden,
    If,
    Immutable,
    NotAllowedFromFront,
    Update,
  )

data User a = User
  { firstName :: !(Field a '[Immutable] Text),
    lastName :: !(Field a '[Immutable] Text),
    login :: !(Field a '[Immutable, AuthField] Text),
    token :: !(Field a '[NotAllowedFromFront, Hidden] Text),
    password :: !(Field a '[Immutable, Hidden, AuthField] Text),
    registered :: !(Field a '[Immutable, NotAllowedFromFront] Date),
    admin :: !(Field a '[Immutable] Bool)
  }
  deriving stock (Generic)

instance
  {-# OVERLAPPING #-}
  (GL.HasField' name (User f) a, f ~ g, a ~ b) =>
  GL.HasField name (User f) (User g) a b
  where
  field = GL.field' @name

deriving instance
  ( Data a,
    Data (Field a '[Immutable] Text),
    Data (Field a '[Immutable] Text),
    Data (Field a '[Immutable, AuthField] Text),
    Data (Field a '[NotAllowedFromFront, Hidden] Text),
    Data (Field a '[Immutable, Hidden, AuthField] Text),
    Data (Field a '[Immutable, NotAllowedFromFront] Date),
    Data (Field a '[Immutable] Bool)
  ) =>
  Data (User a)

deriving instance
  ( Show (Field a '[Immutable] Text),
    Show (Field a '[Immutable] Text),
    Show (Field a '[Immutable, AuthField] Text),
    Show (Field a '[NotAllowedFromFront, Hidden] Text),
    Show (Field a '[Immutable, Hidden, AuthField] Text),
    Show (Field a '[Immutable, NotAllowedFromFront] Date),
    Show (Field a '[Immutable] Bool)
  ) =>
  Show (User a)

deriving instance
  ( Eq (Field a '[Immutable] Text),
    Eq (Field a '[Immutable] Text),
    Eq (Field a '[Immutable, AuthField] Text),
    Eq (Field a '[NotAllowedFromFront, Hidden] Text),
    Eq (Field a '[Immutable, Hidden, AuthField] Text),
    Eq (Field a '[Immutable, NotAllowedFromFront] Date),
    Eq (Field a '[Immutable] Bool)
  ) =>
  Eq (User a)

data AuthField

data Auth

type instance
  Field Auth modifiers a =
    If
      (Contains AuthField modifiers)
      a
      (Maybe NotAllowedFromFront)

aesonOpts :: A.Options
aesonOpts =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = camelTo2 '_'
    }

-- | Post / Create
instance FromJSON (User (Front Create)) where
  parseJSON = genericParseJSON aesonOpts

deriving instance Postgres.ToRow (User Create)

instance Database.Postable User

-- | Get / Front Display
instance ToJSON (User (Front Display)) where
  toJSON = genericToJSON aesonOpts

deriving instance Postgres.FromRow (User (Front Display))

instance Database.Gettable (Entity User) (Front Display)

-- | Put / Update on Auth
deriving instance FromJSON (User Auth)

deriving instance Postgres.FromRow (User Display)

deriving instance EmptyData (User Update)

deriving instance Postgres.ToRow (User Update)

instance Database.Puttable (User Update)

instance Database.Gettable (Entity User) Display

-- | Delete
instance Database.Deletable User

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module Entity.User where

import App.Types (Date)
import Data.Aeson (FromJSON (..), ToJSON (..), camelTo2, defaultOptions, fieldLabelModifier, genericParseJSON, genericToJSON, omitNothingFields)
import Data.Aeson qualified as A
import Data.Data (Data)
import Data.Kind (Type, Constraint)
import Data.Generics.Product.Fields qualified as GL
import Data.Text (Text)
import Database.Delete qualified as Database
import Database.Get qualified as Database
import Database.Post qualified as Database
import Database.Put qualified as Database
import Entity.Internal (Entity(..))
import Extended.Postgres qualified as Postgres
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
    admin :: !(Field a '[Immutable, NotAllowedFromFront] Bool)
  }
  deriving stock (Generic)

instance
  {-# OVERLAPPING #-}
  (GL.HasField' name (User f) a, f ~ g, a ~ b) =>
  GL.HasField name (User f) (User g) a b
  where
  field = GL.field' @name

type UserFieldsConstraint a (constr :: Type -> Constraint) =
  ( constr (Field a '[Immutable] Text),
    constr (Field a '[Immutable] Text),
    constr (Field a '[Immutable, AuthField] Text),
    constr (Field a '[NotAllowedFromFront, Hidden] Text),
    constr (Field a '[Immutable, Hidden, AuthField] Text),
    constr (Field a '[Immutable, NotAllowedFromFront] Date),
    constr (Field a '[Immutable, NotAllowedFromFront] Bool)
  )

deriving instance
  ( Data a,
    UserFieldsConstraint a Data
  ) =>
  Data (User a)

deriving instance
  ( UserFieldsConstraint a Show
  ) =>
  Show (User a)

deriving instance
  ( UserFieldsConstraint a Eq
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

instance Postgres.ToRow (Entity User Update) where
  toRow Entity {..} = Postgres.toRow entity ++ Postgres.toRow entityID

instance Database.Puttable (User Update)

instance Database.Gettable (Entity User) Display

-- | Delete
instance Database.Deletable User

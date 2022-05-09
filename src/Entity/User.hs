{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Entity.User where

import Data.Aeson (FromJSON(..), ToJSON(..), defaultOptions, omitNothingFields, genericToJSON, fieldLabelModifier, camelTo2, genericParseJSON)
import Data.Aeson qualified as A
import Data.Text (Text)
import Data.Generics.Product.Fields qualified as GL
import GHC.Generics ( Generic )


import HKD.HKD


import Database.PostgreSQL.Simple qualified as Postgres

import Database.Database qualified as Database

import Postgres.Internal

import Data.Data

import Entity.Internal

import App.Types
import App.Internal

data User a = User
  { firstName  :: Field  a '[Immutable]                       Text
  , lastName   :: Field  a '[Immutable]                       Text
  , login      :: Field  a '[Immutable, AuthField]            Text
  , token      :: Field  a '[NotAllowedFromFront, Hidden]     Text
  , password   :: Field  a '[Immutable, Hidden, AuthField]    Text
  , registered :: Field  a '[Immutable, NotAllowedFromFront]  Date
  , admin      :: Field  a '[Immutable]                       Bool 
  } deriving stock (Generic)
  
instance
  {-# OVERLAPPING #-}
  (GL.HasField' name (User f) a, f ~ g, a ~ b) =>
  GL.HasField name (User f) (User g) a b
  where
  field = GL.field' @name

deriving instance 
    ( Data a
    , Data (Field  a '[Immutable]                       Text)
    , Data (Field  a '[Immutable]                       Text)
    , Data (Field  a '[Immutable, AuthField]            Text)
    , Data (Field  a '[NotAllowedFromFront, Hidden]     Text)
    , Data (Field  a '[Immutable, Hidden, AuthField]    Text)
    , Data (Field  a '[Immutable, NotAllowedFromFront]  Date)
    , Data (Field  a '[Immutable]                       Bool)
    ) => Data (User a)

deriving instance 
    ( Show (Field  a '[Immutable]                       Text)
    , Show (Field  a '[Immutable]                       Text)
    , Show (Field  a '[Immutable, AuthField]            Text)
    , Show (Field  a '[NotAllowedFromFront, Hidden]     Text)
    , Show (Field  a '[Immutable, Hidden, AuthField]    Text)
    , Show (Field  a '[Immutable, NotAllowedFromFront]  Date)
    , Show (Field  a '[Immutable]                       Bool)
    ) => Show (User a)

deriving instance 
    ( Eq (Field  a '[Immutable]                       Text)
    , Eq (Field  a '[Immutable]                       Text)
    , Eq (Field  a '[Immutable, AuthField]            Text)
    , Eq (Field  a '[NotAllowedFromFront, Hidden]     Text)
    , Eq (Field  a '[Immutable, Hidden, AuthField]    Text)
    , Eq (Field  a '[Immutable, NotAllowedFromFront]  Date)
    , Eq (Field  a '[Immutable]                       Bool)
    ) => Eq (User a)

data AuthField

data Auth

type instance Field Auth modifiers a = 
    If (Contains AuthField modifiers) 
       a
       (Maybe NotAllowedFromFront)

aesonOpts :: A.Options
aesonOpts = defaultOptions 
        { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

-- | Post / Create
instance FromJSON                             (User (Front Create)) where
    parseJSON = genericParseJSON aesonOpts
deriving instance Postgres.ToRow              (User Create)

-- | Get / Front Display
instance ToJSON                    (User (Front Display)) where
    toJSON = genericToJSON aesonOpts
deriving instance Postgres.FromRow (User (Front Display))

-- | Put / Update on Auth
deriving instance FromJSON         (User Auth)
deriving instance Postgres.FromRow (User Display)
deriving instance EmptyData        (User Update)
deriving instance Postgres.ToRow   (User Update)



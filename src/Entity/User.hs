{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Entity.User where

import Data.Aeson (FromJSON(..), ToJSON(..), omitNothingFields, defaultOptions, genericToJSON, fieldLabelModifier, camelTo2, genericParseJSON)
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
  { firstName  :: Field 'Required a '[Immutable]                       Text
  , lastName   :: Field 'Required a '[Immutable]                       Text
  , login      :: Field 'Required a '[Immutable, Authfield]            Text
  , token      :: Field 'Required a '[NotAllowedFromFront, Hidden]     Text
  , password   :: Field 'Required a '[Immutable, Hidden, Authfield]    Text
  , registered :: Field 'Required a '[Immutable, NotAllowedFromFront]  Date
  , admin      :: Field 'Required a '[Immutable]                       Bool 
  } deriving stock (Generic)
  
instance
  {-# OVERLAPPING #-}
  (GL.HasField' name (User f) a, f ~ g, a ~ b) =>
  GL.HasField name (User f) (User g) a b
  where
  field = GL.field' @name

-- data TU

-- data TokenUpdate
--     deriving Data

-- type instance Field req TokenUpdate modifiers a =
--     If (Contains TU modifiers)
--        (Maybe a)
--        (Maybe NotUpdated)

data Authfield

data Auth

type instance Field req Auth modifiers a = 
    If (Contains Authfield modifiers) 
       (Field req Create modifiers a)
       (Maybe NotAllowedFromFront)

-- | Post / Create
instance FromJSON                             (User (Front Create)) where
    parseJSON = genericParseJSON defaultOptions 
        { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }
deriving instance Show                        (User Create)
deriving instance Data                        (User Create)
deriving instance Postgres.ToRow              (User Create)

-- | Get / Front Display
deriving instance Show             (User (Front Display))
deriving instance Data             (User (Front Display))
instance ToJSON                    (User (Front Display)) where
    toJSON = genericToJSON defaultOptions 
        { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }
deriving instance Postgres.FromRow (User (Front Display))

-- | Put / Update on Auth
deriving instance FromJSON         (User  Auth)
deriving instance Show             (User  Display)
deriving instance Data             (User  Display)
deriving instance Postgres.FromRow (User  Display)
deriving instance EmptyData        (User  Update)
deriving instance Data             (User  Update)
deriving instance Postgres.ToRow   (User  Update)

-- | Delete
deriving instance Data             (User  Delete)



-- instance Database.PuttableTo Postgres (Entity User) Update

-- instance Database.GettableFrom Postgres  User (Front Display) where

--     type Filters User (Front Display) = Database.Pag


-- deriving instance Show (User (Front Display))

-- deriving instance Data             (User Create)
-- deriving instance Show             (User Create)
-- deriving instance Data             (User (Front Create))
-- deriving instance FromJSON         (User (Front Create))
-- deriving instance Postgres.ToRow   (User Create)
-- deriving instance Database.PostableTo Postgres User

-- deriving instance Show                          (User Display)
-- deriving instance Data                          (User Display)
-- deriving instance Postgres.FromRow              (User Display)
-- deriving instance Database.GettableFrom Postgres User Display

-- instance ToJSON                    (User (Front Display)) where
--     toJSON = genericToJSON defaultOptions { omitNothingFields = True }
    
-- deriving instance Postgres.FromRow (User (Front Display))
-- deriving instance Data             (User (Front Display))
-- instance Database.GettableFrom Postgres User  (Front Display) where
--     getQuery = mconcat
--         [ "SELECT ", fieldsQuery @(User (Front Display))
--         , " FROM users" ]

-- instance Database.GettableFrom Postgres (Entity User) Display where
--     getQuery = mconcat
--         [ "SELECT id, ", fieldsQuery @(User Display)
--         , " FROM users" ]

-- instance Database.PuttableTo Postgres (Entity User) Update where

--     putQuery = "UPDATE users SET token = ? WHERE id = ?"

-- deriving instance Data (User Update)

-- instance Database.ToOneRow (User TokenUpdate) IDs where 

--     type instance MkOneRow (User TokenUpdate) IDs 
--         = (Maybe Text, ID (Path Current)) 

--     toOneRow User{..} [uID] = pure (token, uID)
--     toOneRow _ _  = entityIDArityMissmatch "user token update"

-- deriving instance EmptyData (User TokenUpdate)
-- deriving instance Data (User TokenUpdate)

-- deriving instance Postgres.ToRow (User TokenUpdate)
        


-- deriving instance Data (User Delete)

-- deriving instance FromJSON (User Auth)



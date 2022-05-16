module Helpers.AuthorDB where

import App.Types (ID (ID))
import Control.Lens ((.~))
import Control.Monad.State (gets, modify)
import Data.Coerce (coerce)
import Data.IntMap qualified as IM
import Data.Maybe (fromMaybe)
import Entity.Author (Author (..))
import Entity.Internal (Entity (..))
import Entity.User (User (..))
import Extended.Text (Text)
import HKD.HKD (Create, Display, Front, Update)
import Helpers.Database
  ( TestEntity
      ( alreadyExists,
        extractTestDatabaseFromTestState,
        fromDisplay,
        getFromTestDatabase,
        getTestDatabase,
        putDatabase,
        putIntoTestDatabase,
        toDisplay,
        toFrontCreate,
        withTestDatabase
      ),
    TestUpdate (..),
    getManyOrSingle,
    putEntityIntoTestDatabase,
  )
import Helpers.Internal
import Helpers.Monad (TestState (_tsAuthorDB), tsAuthorDB)
import Helpers.UserDB ()

-- | Post
instance TestEntity (Author Create) where
  getTestDatabase = gets _tsAuthorDB

  alreadyExists a db =
    not $
      IM.null $
        IM.filter
          ((== coerce (user a)) . entityID . user)
          db

  toDisplay Author {..} =
    let u =
          User
            { firstName = "",
              lastName = "",
              login = "",
              token = "",
              password = "",
              registered = testDate,
              admin = False
            }
     in Author {user = Entity (coerce user) u, ..}

  toFrontCreate Author {..} = Author {user = coerce user, ..}

  fromDisplay Author {..} = Author {user = coerce $ entityID user, ..}

  extractTestDatabaseFromTestState = _tsAuthorDB

-- | Get
instance (TestEntity (Entity Author (Front Display))) where
  getFromTestDatabase = getManyOrSingle

instance TestEntity (Author (Front Display)) where
  fromDisplay Author {user = Entity {..}, ..} =
    Author
      { user =
          Entity
            { entityID = coerce entityID,
              entity = fromDisplay entity
            },
        ..
      }

instance TestEntity (Author Display) where
  extractTestDatabaseFromTestState = _tsAuthorDB

  alreadyExists a db =
    let f = entityID . user
     in not $ IM.null $ IM.filter ((== f a) . f) db

  getTestDatabase = gets _tsAuthorDB

  putDatabase db = modify (tsAuthorDB .~ db)

  withTestDatabase db = tsAuthorDB .~ db

  toFrontCreate Author {..} = Author {user = coerce $ entityID user, ..}

-- | Put
instance TestEntity (Author (Front Update)) where
  extractTestDatabaseFromTestState = _tsAuthorDB

instance TestUpdate (Author (Front Update)) where
  testUpdate au ad =
    Author
      { description = fromMaybe (description ad) (description au),
        user = user ad
      }

instance TestEntity (Entity Author (Front Update)) where
  putIntoTestDatabase = putEntityIntoTestDatabase

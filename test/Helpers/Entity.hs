module Helpers.Entity where

import Entity.Author
import Entity.Category
import Entity.Tag
import Entity.User
import HKD.HKD
import Helpers.Author
import Helpers.Category
import Helpers.Database
import Helpers.Monad
import Helpers.User
import Unsafe.Coerce

-- | Class for generic props

-- class TestEntity e where
--     eDisplayToFrontDisplay :: e Display -> e (Front Display)
--     eDisplayToFrontCreate  :: e Display -> e (Front Create)
--     withDatabase           ::  TDB e -> StateMod
--     dbFromTestState        ::  TestState -> EMap (e Display)

-- instance TestEntity Author where
--     eDisplayToFrontDisplay = authorDisplayToAuthorFrontDisplay
--     withDatabase db TestState{..} = TestState{authorDB = db, ..}
--     dbFromTestState TestState{..} = authorDB

-- instance TestEntity User where
--     eDisplayToFrontDisplay = userDisplayToUserFrontDisplay
--     withDatabase db TestState{..} = TestState{userDB = db, ..}
--     dbFromTestState TestState{..} = userDB

-- instance TestEntity Tag where
--     eDisplayToFrontDisplay        = unsafeCoerce
--     eDisplayToFrontCreate         = unsafeCoerce
--     withDatabase db TestState{..} = TestState{tagDB = db, ..}
--     dbFromTestState TestState{..} = tagDB

-- instance TestEntity Category where
--     eDisplayToFrontDisplay        = error "catDisplayToFrontDisplay"
--     eDisplayToFrontCreate         = unsafeCoerce
--     withDatabase db TestState{..} = TestState{catDB = db, ..}
--     dbFromTestState TestState{..} = catDB

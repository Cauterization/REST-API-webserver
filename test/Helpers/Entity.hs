module Helpers.Entity where

import Entity.Author
import Entity.User

import Helpers.Database
import Helpers.Author
import Helpers.User 
import Helpers.Monad

import HKD.HKD


class TestEntity e where
    eDisplayToFrontDisplay :: e Display -> e (Front Display)
    withDatabase           ::  TDB e -> StateMod
    dbFromTestState        ::  TestState -> EMap (e Display)

instance TestEntity Author where
    eDisplayToFrontDisplay = authorDisplayToAuthorFrontDisplay
    withDatabase db TestState{..} = TestState{authorDB = db, ..}
    dbFromTestState TestState{..} = authorDB

instance TestEntity User where
    eDisplayToFrontDisplay = userDisplayToUserFrontDisplay
    withDatabase db TestState{..} = TestState{userDB = db, ..}
    dbFromTestState TestState{..} = userDB

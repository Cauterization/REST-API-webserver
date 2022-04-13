module Helpers.Update where

import Entity.Author
import Entity.Tag
import HKD.HKD
import Data.Maybe (fromMaybe)

infixl 4 >/

(>/) :: TestUpdate e => e (Front Update) -> e Display -> e Display
(>/) = testUpdate

class TestUpdate e where
    testUpdate :: e (Front Update) -> e Display -> e Display

(>\) :: Maybe a -> a -> a
(>\) = flip fromMaybe  

instance TestUpdate Author where
    testUpdate au a = Author
        { description = description au >\ description a
        , user        = user a
        }

instance TestUpdate Tag where
    testUpdate tu t = Tag $ tag tu >\ tag t
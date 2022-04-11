{-# LANGUAGE GeneralisedNewtypeDeriving#-}

module Helpers.Tag where

import Data.Aeson
import Data.Text (Text)
import Data.Text qualified as T
import Entity.Tag
import Helpers.Internal
import HKD.HKD

import Test.QuickCheck
import Unsafe.Coerce

tagDisplayToFrontDisplay :: Tag Display -> Tag (Front Display)
tagDisplayToFrontDisplay = unsafeCoerce

deriving instance Eq                 (Tag Create)
deriving instance Ord                (Tag Create)
deriving anyclass instance ToJSON    (Tag Create)  
deriving newtype instance  Arbitrary (Tag Create)

deriving anyclass instance ToJSON    (Tag (Front Create))
deriving anyclass instance FromJSON  (Tag (Front Create))

deriving instance Show              (Tag Display)
deriving instance Eq                (Tag Display)
deriving instance Ord               (Tag Display)
deriving newtype instance Arbitrary (Tag Display)


deriving instance Eq                (Tag (Front Display))
deriving instance Ord               (Tag (Front Display))
deriving anyclass instance FromJSON (Tag (Front Display))

deriving instance Show              (Tag (Front Update))
deriving anyclass instance ToJSON   (Tag (Front Update))   
deriving newtype instance Arbitrary (Tag (Front Update))
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Mocks.Utils where

import App.Types
import Data.Typeable
import Extended.Text (Text)
import Extended.Text qualified as T
import Data.Aeson
import HKD.HKD
import Test.QuickCheck

mkPathFromID :: forall e a. Typeable e => ID (e a) -> Text
mkPathFromID eID = T.pack (withPluralEnding (nameOf @e)) <> "/" <> T.show eID

instance {-# OVERLAPPING #-} Show (Maybe NotUpdated) where
  show _ = "Nothing"

instance {-# OVERLAPPING #-} ToJSON (Maybe NotAllowedFromFront) where
  toJSON _ = Null

instance {-# OVERLAPPING #-} ToJSON (Maybe NotUpdated) where
  toJSON _ = Null

instance {-# OVERLAPPING #-} Arbitrary (Maybe NotAllowedFromFront) where
  arbitrary = pure Nothing

instance {-# OVERLAPPING #-} Arbitrary (Maybe NotUpdated) where
  arbitrary = pure Nothing

instance {-# OVERLAPPING #-} Arbitrary (Maybe NotDisplayed) where
  arbitrary = pure Nothing
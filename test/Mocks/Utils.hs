{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module Mocks.Utils where

import App.Types (ID, nameOf, withPluralEnding)
import Data.Aeson (ToJSON (toJSON), Value (Null))
import Data.Typeable (Typeable)
import Extended.Text (Text)
import Extended.Text qualified as T
import HKD.HKD (NotAllowedFromFront, NotDisplayed, NotUpdated)
import Test.QuickCheck (Arbitrary (arbitrary))

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

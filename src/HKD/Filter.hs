{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE BlockArguments #-}
module HKD.Filter where

import HKD.EmptyData
import HKD.Field
import HKD.Utils
import Data.Aeson qualified as J 
import Data.Aeson.Types qualified as J 
import GHC.Generics

data Filter

data CustomFilter a 

data ItSelf
data Exists a = Exists a | DoesNotExist
    deriving stock (Generic, Show)

instance {-# OVERLAPPING #-} J.FromJSON (Exists (NotFiltered a)) where
    parseJSON = parseExists (const $ pure NotFiltered)

instance {-# OVERLAPPABLE #-} J.FromJSON a => J.FromJSON (Exists a) where 
    parseJSON = parseExists J.parseJSON

parseExists :: (J.Value -> J.Parser a) -> J.Value -> J.Parser (Exists a)
parseExists f = J.withObject "Exists" \obj -> do
    obj J..: "tag" >>= \case
      "Exists" -> Exists <$> ((obj J..: "content") >>= f)
      "DoesNotExist" ->  pure DoesNotExist
      s -> J.unexpected $ J.String s

data NotFiltered a = NotFiltered
    deriving stock (Generic, Show)

instance J.FromJSON (NotFiltered a) where
    parseJSON _ = fail "Can't filter on this Field"

type family ApplyFilter qs a where
  ApplyFilter (CustomFilter q ': qs) a = Maybe (Exists (q a))
  ApplyFilter (nq ': qs)             a = ApplyFilter qs a
  ApplyFilter '[]                    a = Maybe (Exists [a])  

type instance Field Filter modifiers a = 
  If (Contains (CustomFilter ItSelf) modifiers) 
     (Maybe (Exists a)) 
     (ApplyFilter modifiers a)
  
query :: EmptyData (e Filter) => (e Filter -> e Filter) -> e Filter
query = ($ emptyData)
module Database.Query where

import Data.String (IsString(..), fromString)
import Data.Maybe (isNothing, catMaybes)
import Data.Char (toUpper)
import Data.List (intersperse)

type family Query db :: *

-- | newtype for database queries
-- | an associated type is needed here in order 
-- | to be able to make query a class method

data EQuery db e = EQuery
    { eqSELECT :: Maybe (Query db)
    , eqFROM   :: Maybe (Query db)
    , eqWHERE  :: Maybe (Query db)
    , eqLIMIT  :: Maybe (Query db)
    , eqOFFSET :: Maybe (Query db)
    }

eqParts :: forall db e a. IsString a => [a]
eqParts = ["SELECT", "FROM", "WHERE", "LIMIT", "OFFSET"]

qSELECT :: forall db e. QConstraints db => Query db -> EQuery db e
qSELECT q = (\EQuery{..} -> EQuery{eqSELECT = Just q, .. }) $ mempty @(EQuery db e)

qFROM :: forall db e. QConstraints db => Query db -> EQuery db e
qFROM q = (\EQuery{..} -> EQuery{eqFROM = Just q, .. }) $ mempty @(EQuery db e)

qWHERE :: forall db e. QConstraints db => Query db -> EQuery db e
qWHERE q = (\EQuery{..} -> EQuery{eqWHERE = Just q, .. }) $ mempty @(EQuery db e)

qLIMIT :: forall db e. QConstraints db => Query db -> EQuery db e
qLIMIT q = (\EQuery{..} -> EQuery{eqLIMIT = Just q, .. }) $ mempty @(EQuery db e)

qOFFSET :: forall db e. QConstraints db => Query db -> EQuery db e
qOFFSET q = (\EQuery{..} -> EQuery{eqOFFSET = Just q, .. }) $ mempty @(EQuery db e)


deriving instance Show (Query db) => Show (EQuery db e)

instance IsString (Query db) => IsString (EQuery db e) where
    fromString str = EQuery eqs eqf eqw eql eqo
      where
        g (q:qs) (s:ss) = 
            if q == map toUpper s 
            then let (from, to) = break (`elem` qs) ss
                 in unwords from : g qs to
            else [] : g qs (s:ss)
        g (q:qs) [] = "" : g qs []
        g []     res = res
        eqs : eqf : eqw : eql : eqo : _ = map (Just . fromString) $ g eqParts (words str)

instance (Semigroup (Query db), IsString (Query db)) => Semigroup (EQuery db e) where
    EQuery s1 f1 w1 l1 o1 <> EQuery s2 f2 w2 l2 o2 = EQuery 
        (g ", "    s1 s2) 
        (g ", "    f1 f2) 
        (g " AND " w1 w2) 
        (g " "     l1 l1)
        (g " "     o1 o2)
      where
        g q a b | isNothing a = b
                | isNothing b = a
                | otherwise   = a <> Just q <> b 

instance (Semigroup (Query db), IsString (Query db)) => Monoid (EQuery db e) where
    mempty = EQuery Nothing Nothing Nothing Nothing Nothing

type QConstraints db = (IsString (Query db), Monoid (Query db))

unEQuery :: forall db e. QConstraints db => EQuery db e -> Query db
unEQuery EQuery{..} = mconcat $ intersperse " " $ catMaybes $
    zipWith f (map pure eqParts) [eqSELECT, eqFROM, eqWHERE, eqLIMIT, eqOFFSET]
  where 
    f w q | isNothing q = q
          | otherwise = w <> Just " " <> q

g :: [[Char]] -> [[Char]] -> [String]
g (q:qs) (s:ss) = 
    if q == map toUpper s 
    then let (from, to) = break (`elem` qs) ss
         in unwords from : g qs to
    else [] : g qs (s:ss)
g (q:qs) [] = "" : g qs []
g []     res = res


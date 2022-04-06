module Database.Query where

import Data.String (IsString(..), fromString)
import Data.Maybe (isNothing, catMaybes)
import Data.Char (toUpper)
import Data.List (intersperse)

import Database.Internal

-- type family Query db :: *

-- | newtype for database queries
-- | an associated type is needed here in order 
-- | to be able to make query a class method

data Query db e = Query
    { eqSELECT :: Maybe (QueryOf db)
    , eqFROM   :: Maybe (QueryOf db)
    , eqWHERE  :: Maybe (QueryOf db)
    , eqLIMIT  :: Maybe (QueryOf db)
    , eqOFFSET :: Maybe (QueryOf db)
    }

eqParts :: forall db e a. IsString a => [a]
eqParts = ["SELECT", "FROM", "WHERE", "LIMIT", "OFFSET"]

qSELECT :: forall db e. QConstraints db => QueryOf db -> Query db e
qSELECT q = (\Query{..} -> Query{eqSELECT = Just q, .. }) $ mempty @(Query db e)

qFROM :: forall db e. QConstraints db => QueryOf db -> Query db e
qFROM q = (\Query{..} -> Query{eqFROM = Just q, .. }) $ mempty @(Query db e)

qWHERE :: forall db e. QConstraints db => QueryOf db -> Query db e
qWHERE q = (\Query{..} -> Query{eqWHERE = Just q, .. }) $ mempty @(Query db e)

qLIMIT :: forall db e. QConstraints db => QueryOf db -> Query db e
qLIMIT q = (\Query{..} -> Query{eqLIMIT = Just q, .. }) $ mempty @(Query db e)

qOFFSET :: forall db e. QConstraints db => QueryOf db -> Query db e
qOFFSET q = (\Query{..} -> Query{eqOFFSET = Just q, .. }) $ mempty @(Query db e)

deriving instance Show (QueryOf db) => Show (Query db e)

instance (IsString (QueryOf db), Monoid (QueryOf db)) 
    => IsString (Query db e) where
    fromString str = case map (Just . fromString) $ g eqParts (words str) of
            eqs : eqf : eqw : eql : eqo : _ -> Query eqs eqf eqw eql eqo
            _ -> mempty
      where
        g (q:qs) (s:ss) = 
            if q == map toUpper s 
            then let (from, to) = break (`elem` qs) ss
                 in unwords from : g qs to
            else [] : g qs (s:ss)
        g (q:qs) [] = "" : g qs []
        g []     res = res
        

instance (Semigroup (QueryOf db), IsString (QueryOf db)) => Semigroup (Query db e) where
    Query s1 f1 w1 l1 o1 <> Query s2 f2 w2 l2 o2 = Query 
        (g ", "    s1 s2) 
        (g ", "    f1 f2) 
        (g " AND " w1 w2) 
        (g " "     l1 l1)
        (g " "     o1 o2)
      where
        g q a b | isNothing a = b
                | isNothing b = a
                | otherwise   = a <> Just q <> b 

instance (Semigroup (QueryOf db), IsString (QueryOf db)) => Monoid (Query db e) where
    mempty = Query Nothing Nothing Nothing Nothing Nothing

type QConstraints db = (IsString (QueryOf db), Monoid (QueryOf db), Show (QueryOf db))

unQuery :: forall db e. QConstraints db => Query db e -> QueryOf db
unQuery Query{..} = mconcat $ intersperse " " $ catMaybes $
    zipWith f (map pure eqParts) [eqSELECT, eqFROM, eqWHERE, eqLIMIT, eqOFFSET]
  where 
    f w q | isNothing q = q
          | otherwise = w <> Just " " <> q

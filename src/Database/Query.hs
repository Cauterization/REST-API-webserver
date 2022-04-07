{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Database.Query where

import Data.String (IsString(..), fromString)
import Data.Maybe (isNothing, catMaybes, fromMaybe)
import Data.Char (toUpper)
import Data.List (intersperse, intercalate)

import Database.Internal
import Text.Read (readEither)

-- | A newtype for database queries
-- an associated type is needed here in order 
-- to be able to make query a class method

newtype Query db e = Query {unQuery :: QueryOf db}

deriving newtype instance IsString (QueryOf db) => IsString (Query db e)
deriving newtype instance Semigroup (QueryOf db) => Semigroup (Query db e)
deriving newtype instance Monoid (QueryOf db) => Monoid (Query db e)

type QConstraints db = (IsString (QueryOf db), Monoid (QueryOf db), Show (QueryOf db))

-- data Command db 
--     = INSERT (QueryOf db) 
--     | SELECT (QueryOf db) 
--     | UPDATE (QueryOf db) 
--     | DELETE (QueryOf db) 
--     | Empty
--     | Failed

-- getCommand :: IsString (QueryOf db) => String -> Command db
-- getCommand str = case takeWhile (`notElem` noncommandHeaders) $ words str of
--     "INSERT" : xs -> INSERT $ fromString $ unwords xs
--     "SELECT" : xs -> SELECT $ fromString $ unwords xs
--     "UPDATE" : xs -> UPDATE $ fromString $ unwords xs
--     "DELETE" : xs -> DELETE $ fromString $ unwords xs
--     _ -> Empty

-- getLimit :: (Read a, Bounded a) => String -> a
-- getLimit str = case drop 1 $ dropWhile (/= "LIMIT") $ words str of
--     x:xs -> case readEither x of
--         Right i -> i
--         Left err -> maxBound
--     [] -> maxBound

-- instance Semigroup (Command db) where
--     a <> Empty = a
--     Empty <> a = a
--     a <> b = Failed

-- instance Monoid (Command db) where
--     mempty = Empty


-- deriving instance Show (QueryOf db) => Show (Command db)

-- data Query db e = Query
--     { eqCOMMAND :: Command db
--     , eqTABLE  :: Maybe (QueryOf db)
--     , eqWHERE  :: Maybe (QueryOf db)
--     , eqLIMIT  :: Int
--     , eqOFFSET :: Maybe (QueryOf db)
--     }

-- noncommandHeaders :: forall db e a. IsString a => [a]
-- noncommandHeaders = ["FROM", "INTO", "WHERE", "LIMIT", "OFFSET"]

-- qSELECT :: forall db e. QConstraints db => QueryOf db -> Query db e
-- qSELECT q = (\Query{..} -> Query{eqCOMMAND = SELECT q, .. }) $ mempty @(Query db e)

-- qINSERT :: forall db e. QConstraints db => QueryOf db -> Query db e
-- qINSERT q = (\Query{..} -> Query{eqCOMMAND = INSERT q, .. }) $ mempty @(Query db e)

-- qTABLE :: forall db e. QConstraints db => QueryOf db -> Query db e
-- qTABLE q = (\Query{..} -> Query{eqTABLE = Just q, .. }) $ mempty @(Query db e)

-- qWHERE :: forall db e. QConstraints db => QueryOf db -> Query db e
-- qWHERE q = (\Query{..} -> Query{eqWHERE = Just q, .. }) $ mempty @(Query db e)

-- qLIMIT :: forall db e. QConstraints db => Int -> Query db e
-- qLIMIT q = (\Query{..} -> Query{eqLIMIT = q, .. }) $ mempty @(Query db e)

-- qOFFSET :: forall db e. QConstraints db => QueryOf db -> Query db e
-- qOFFSET q = (\Query{..} -> Query{eqOFFSET = Just q, .. }) $ mempty @(Query db e)

-- deriving instance Show (QueryOf db) => Show (Query db e)

-- -- instance (IsString (QueryOf db), Monoid (QueryOf db)) 
-- --     => IsString (Query db e) where
-- --     fromString str = case map (Just . fromString) $ g noncommandHeaders (words str) of
-- --             eqs : eqf : eqw : eql : eqo : _ -> 
-- --                 Query (getCommand str) eqf eqw (getLimit str) eqo
-- --             _ -> mempty
-- --       where
-- --         g (q:qs) (s:ss) = 
-- --             if q == map toUpper s 
-- --             then let (from, to) = break (`elem` qs) ss
-- --                  in unwords from : g qs to
-- --             else [] : g qs (s:ss)
-- --         g (_:qs) [] = "" : g qs []
-- --         g []     res = res
        

-- instance (QConstraints db) => Semigroup (Query db e) where
--     Query s1 f1 w1 l1 o1 <> Query s2 f2 w2 l2 o2 = Query 
--         (s1 <> s2) 
--         (g ", "    f1 f2) 
--         (g " AND " w1 w2) 
--         (min l1 l2)
--         (g " "     o1 o2)
--       where
--         g q a b | isNothing a = b
--                 | isNothing b = a
--                 | otherwise   = a <> Just q <> b 

-- instance (QConstraints db) => Monoid (Query db e) where
--     mempty = Query Empty Nothing Nothing maxBound Nothing

-- 

-- unQuery :: QConstraints db => Query db e -> QueryOf db
-- unQuery Query{..} = case eqCOMMAND of
--     INSERT q -> 
--             "INSERT " <> 
--             maybe   "" ( " INTO " <>) eqTABLE <> "s " <>
--             "(" <> q <> ")" <> " VALUES " <> 
--             "(" <>
--             fromString (intercalate "," 
--                 $ flip replicate "?" $ (+1) $ length $ filter (== ',') $ show q)
--             <> ")"

--     SELECT q -> 
--             "SELECT " <> q <> 
--             maybe "" ( " FROM " <>) eqTABLE <> 
--             maybe "" (" WHERE " <>) eqWHERE <>
--             (if eqLIMIT == maxBound then "" else " LIMIT " <> fromString (show eqLIMIT)) <>
--             maybe "" (" OFFSET " <>) eqOFFSET
--     _ -> undefined



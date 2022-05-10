{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
module Helpers.ArticleDB where

import App.Types
import App.Internal

import Control.Arrow
import Control.Monad.Catch
import Control.Monad.State
import Control.Lens

import Data.Coerce
import Data.Data
import Data.List
import Database.Database
import Data.IntMap qualified as IM

import Entity.Article
import Entity.Author
import Entity.User
import Entity.Category
import Entity.Tag
import Entity.Internal

import Extended.Text (Text)
import Extended.Text qualified as T

import Helpers.Article
import Helpers.AuthorDB
import Helpers.CategoryDB
import Helpers.TagDB
import Helpers.Database
import Helpers.Monad
import Helpers.UserDB

import HKD.HKD
import Data.Maybe

import Unsafe.Coerce

instance TestEntity (Article Display) where

    withTestDatabase db = tsArticleDB .~ db

    extractTestDatabaseFromTestState = _tsArticleDB

instance TestEntity (Article (Front Display)) where

    fromDisplay Article{..} = Article
        { author = Entity (coerce $ entityID author) $ fromDisplay $ entity author
        , category = Entity (coerce $ entityID category) $ fromDisplay $ entity category
        , tags = map (\(Entity i e) -> Entity (coerce i) (fromDisplay e)) tags
        , pics = map coerce pics
        , .. 
        }

instance TestEntity (Entity Article (Front Display)) where

    getFromTestDatabase = getManyOrSingle

    applyFilters = \case
        [] -> id
        fs -> applyArticleFilters fs

type EAFilter = [Entity Article (Front Display)] -> [Entity Article (Front Display)]

applyArticleFilters :: [EntityFilterParam] -> EAFilter
applyArticleFilters 
    fs@[ crAt
       , crAtLt
       , crAtGt
       , auLogin
       , catID
       , tagID
       , tagIDIn
       , tagIDAll
       , arTitle
       , arContent
       , substr
       , EFPInt limit
       , EFPInt offset
    ] = foldr (.) id 
      $ (take limit :)
      $ (drop offset :) 
      $ zipWith (\f fFun -> if isNothingParam f then id else fFun f) fs fsfuncs
  where
    fsfuncs :: [EntityFilterParam -> EAFilter]
    fsfuncs = 
        [ crAtFun
        , crAtLtFun
        , crAtGtFun
        , auLoginFun
        , catIDFun
        , tagIDFun
        , tagIDInFun
        , tagIDAllFun
        , arTitleFun
        , arContentFun
        , substrFun
        -- , limitFun
        -- , offsetFun
        ]
    isNothingParam = \case
        EFPTextOptional    Nothing -> True
        EFPIntOptional     Nothing -> True
        EFPIntListOptional Nothing -> True
        EFPDateOptional    Nothing -> True
        _                          -> False

crAtFun :: EntityFilterParam -> EAFilter
crAtFun (EFPDateOptional (Just d)) = filter ((== d) . created . entity) 

crAtLtFun :: EntityFilterParam -> EAFilter
crAtLtFun (EFPDateOptional (Just d)) = filter ((<= d) . created . entity) 

crAtGtFun :: EntityFilterParam -> EAFilter
crAtGtFun (EFPDateOptional (Just d)) = filter ((>= d) . created . entity) 

auLoginFun :: EntityFilterParam -> EAFilter
auLoginFun (EFPTextOptional (Just t)) = filter ((== t) . login . entity . user . entity . author . entity) 

catIDFun :: EntityFilterParam -> EAFilter
catIDFun (EFPIntOptional  (Just i))  = filter ((== ID i) . entityID . category . entity) 

tagIDFun :: EntityFilterParam -> EAFilter
tagIDFun (EFPIntOptional  (Just i)) = filter ((ID i `elem`) . map entityID . tags . entity) 

tagIDInFun :: EntityFilterParam -> EAFilter
tagIDInFun (EFPIntListOptional (Just (map ID -> tagIn))) 
    = filter ((any (`elem` tagIn)) . map entityID . tags . entity)

tagIDAllFun :: EntityFilterParam -> EAFilter
tagIDAllFun (EFPIntListOptional (Just (map ID -> tagAll))) 
    = filter ((all (`elem` tagAll)) . map entityID . tags . entity)

arTitleFun :: EntityFilterParam -> EAFilter
arTitleFun (EFPTextOptional (Just t)) = filter ((== t) . title . entity)

arContentFun :: EntityFilterParam -> EAFilter
arContentFun (EFPTextOptional (Just t)) = filter ((t `T.isInfixOf`) . content . entity)

substrFun :: EntityFilterParam -> EAFilter
substrFun (EFPTextOptional (Just t)) = filter (isSubstr t  . entity)
  where
    isSubstr s Article{..} = 
        let l  = login $ entity $ user $ entity $ author
            c  = unCatName $ name $ entity $ category
            cs = map unCatName $ parent $ entity category
            ts = map (tag . entity) tags
        in any (t `T.isInfixOf`) ([content, l, c] <> cs <> ts)
    
limitFun :: EntityFilterParam -> EAFilter
limitFun (EFPInt i) = take i

offsetFun :: EntityFilterParam -> EAFilter
offsetFun (EFPInt i) = drop i

   


    
    


    -- entityFilters = (<> Database.defaultFilters)
    --     [ Database.EFDate    "crAt"
    --     , Database.EFDate    "crAtLt"
    --     , Database.EFDate    "crAtGt"
    --     , Database.EFString  "author_login"
    --     , Database.EFNum     "category_id"
    --     , Database.EFNum     "tag_id"
    --     , Database.EFNumList "tag_in"
    --     , Database.EFNumList "tag_all"
    --     , Database.EFString  "title"
    --     , Database.EFString  "content"
    --     , Database.EFString  "substring"
    --     limit
    --     offset
    --     ]
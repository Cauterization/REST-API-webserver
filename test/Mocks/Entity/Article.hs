{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Mocks.Entity.Article where

import App.Types (Date, ID (ID))
import Control.Monad.State (gets, join)
import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON), Value, object)
import Database.EntityFilters qualified as Database
import Entity.Article (Article (..), ArticleFieldsConstraint)
import Entity.Author (Author (user))
import Entity.Category
  ( Category (name, parent),
    CategoryName (unCatName),
  )
import Entity.Internal (Entity (entity, entityID), EntityOrID)
import Entity.Picture (Picture)
import Entity.Tag (Tag (tag))
import Entity.User (User (login))
import Extended.Text qualified as T
import HKD.HKD
  ( Create,
    Display,
    Field,
    Front,
    Immutable,
    NotAllowedFromFront,
    Update,
  )
import Mocks.Constant (testAddressConstant, testPortConstant)
import Mocks.Entity.Author ()
import Mocks.Entity.Category (getCatNames)
import Mocks.Entity.Tag ()
import Mocks.TestMonad
  ( TestEntity (applyFilters, getFromState, withGetEntities),
    TestState (getArticles),
  )
import Test.QuickCheck (Arbitrary (arbitrary))

deriving instance ToJSON (Article (Front Create))

deriving instance ToJSON (Article (Front Update))

instance ToJSON (Article (Front Display)) where
  toJSON Article {..} =
    object
      [ "category" .= toJSON category,
        "pics" .= toJSON (map picToJSON pics),
        "content" .= toJSON content,
        "created" .= toJSON created,
        "author" .= toJSON author,
        "title" .= toJSON title,
        "tags" .= toJSON tags
      ]

picToJSON :: ID (Picture a) -> Value
picToJSON =
  toJSON
    . ((testAddressConstant <> T.show testPortConstant <> "/pictures/") <>)
    . T.show

instance
  ( ArticleFieldsConstraint a Arbitrary
  ) =>
  Arbitrary (Article a)
  where
  arbitrary = do
    title <- arbitrary
    created <- arbitrary
    content <- arbitrary
    author <- arbitrary
    category <- arbitrary
    tags <- arbitrary
    pics <- arbitrary
    pure Article {..}

type ArticleListEndo = [Entity Article (Front Display)] -> [Entity Article (Front Display)]

instance TestEntity (Entity Article (Front Display)) where
  getFromState = join $ gets getArticles
  withGetEntities articles s = s {getArticles = pure articles}
  applyFilters
    fs@[ crAt,
         crAtLt,
         crAtGt,
         auLogin,
         catID,
         tagID,
         tagIDIn,
         tagIDAll,
         arTitle,
         arContent,
         substr,
         Database.EFPInt limit,
         Database.EFPInt offset
         ] =
      foldr (.) id $
        (take limit :) $
          (drop offset :) $
            zipWith (\f fFun -> if isNothingParam f then id else fFun f) fs fsfuncs
      where
        fsfuncs :: [Database.EntityFilterParam -> ArticleListEndo]
        fsfuncs =
          [ crAtFun,
            crAtLtFun,
            crAtGtFun,
            auLoginFun,
            catIDFun,
            tagIDFun,
            tagIDInFun,
            tagIDAllFun,
            arTitleFun,
            arContentFun,
            substrFun
          ]
        isNothingParam = \case
          Database.EFPTextOptional Nothing -> True
          Database.EFPIntOptional Nothing -> True
          Database.EFPIntListOptional Nothing -> True
          Database.EFPDateOptional Nothing -> True
          _ -> False
  applyFilters [] = id
  applyFilters ds = error $ show ds

crAtFun :: Database.EntityFilterParam -> ArticleListEndo
crAtFun (Database.EFPDateOptional (Just d)) = filter ((== d) . created . entity)

crAtLtFun :: Database.EntityFilterParam -> ArticleListEndo
crAtLtFun (Database.EFPDateOptional (Just d)) = filter ((<= d) . created . entity)

crAtGtFun :: Database.EntityFilterParam -> ArticleListEndo
crAtGtFun (Database.EFPDateOptional (Just d)) = filter ((>= d) . created . entity)

auLoginFun :: Database.EntityFilterParam -> ArticleListEndo
auLoginFun (Database.EFPTextOptional (Just t)) =
  filter ((== t) . login . entity . user . entity . author . entity)

catIDFun :: Database.EntityFilterParam -> ArticleListEndo
catIDFun (Database.EFPIntOptional (Just i)) = filter ((== ID i) . entityID . category . entity)

tagIDFun :: Database.EntityFilterParam -> ArticleListEndo
tagIDFun (Database.EFPIntOptional (Just i)) = filter ((ID i `elem`) . map entityID . tags . entity)

tagIDInFun :: Database.EntityFilterParam -> ArticleListEndo
tagIDInFun (Database.EFPIntListOptional (Just (map ID -> tagIn))) =
  filter (any ((`elem` tagIn) . entityID) . tags . entity)

tagIDAllFun :: Database.EntityFilterParam -> ArticleListEndo
tagIDAllFun (Database.EFPIntListOptional (Just (map ID -> tagAll))) =
  filter (all ((`elem` tagAll) . entityID) . tags . entity)

arTitleFun :: Database.EntityFilterParam -> ArticleListEndo
arTitleFun (Database.EFPTextOptional (Just t)) = filter ((== t) . title . entity)

arContentFun :: Database.EntityFilterParam -> ArticleListEndo
arContentFun (Database.EFPTextOptional (Just t)) =
  filter ((t `T.isInfixOf`) . content . entity)

substrFun :: Database.EntityFilterParam -> ArticleListEndo
substrFun (Database.EFPTextOptional (Just t)) = filter (isSubstr t . entity)
  where
    isSubstr s Article {..} =
      let l = login $ entity $ user $ entity author
          c = unCatName $ name $ entity category
          cs = getCatNames category
          ts = map (tag . entity) tags
       in any (t `T.isInfixOf`) ([content, l, c] <> cs <> ts)

limitFun :: Database.EntityFilterParam -> ArticleListEndo
limitFun (Database.EFPInt i) = take i

offsetFun :: Database.EntityFilterParam -> ArticleListEndo
offsetFun (Database.EFPInt i) = drop i

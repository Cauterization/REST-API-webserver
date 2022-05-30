{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Mocks.Entity.Article where

import App.Types
import Control.Monad.State
import Data.Aeson
import Entity.Article
import Entity.Author
import Entity.Category
import Entity.Internal
import Entity.Picture
import Entity.Tag
import Entity.User
import Extended.Text qualified as T
import HKD.HKD
import Mocks.Arbitrary
import Mocks.Constant
import Mocks.Entity.Author
import Mocks.Entity.Category
import Mocks.Entity.Tag
import Mocks.TestMonad
import Mocks.Utils
import Test.Hspec
import Test.QuickCheck
import qualified Database.EntityFilters as Database

instance ToJSON (Article (Front Display)) where
  toJSON Article{..} = object 
    [ "category" .= toJSON category
    , "pics" .= toJSON (map picToJSON pics)
    , "content" .= toJSON content
    , "created" .= toJSON created
    , "author" .= toJSON author
    , "title" .= toJSON title
    , "tags" .= toJSON tags
    ]

picToJSON :: ID (Picture a) -> Value
picToJSON = toJSON 
  . ((testAddressConstant <> T.show testPortConstant <> "/pictures/") <>) 
  . T.show

-- deriving instance ToJSON (Author (Front Update))
-- instance Arbitrary (Author Create) where
--   arbitrary = Author <$> arbitrary <*> arbitrary

-- instance Arbitrary (Author (Front Create)) where
--   arbitrary = Author <$> arbitrary <*> arbitrary

-- instance Arbitrary (Author (Front Update)) where
--   arbitrary = Author <$> arbitrary <*> arbitrary

instance Arbitrary (Article (Front Display)) where
  arbitrary = do
    title <- arbitrary
    created <- arbitrary
    content <- arbitrary
    author <- arbitrary
    category <- arbitrary
    tags <- arbitrary
    pics <- arbitrary
    pure Article{..}

-- instance TestEntity (Author Create)

-- instance TestEntity (Entity Author (Front Update))

type ArticleListEndo = [Entity Article (Front Display)] -> [Entity Article (Front Display)]

instance TestEntity (Entity Article (Front Display)) where
  getFromState = join $ gets getArticles
  withGetEntities articles s = s {getArticles = pure articles}
  applyFilters fs@[ 
       crAt,
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
auLoginFun (Database.EFPTextOptional (Just t)) = filter ((== t) . login . entity . user . entity . author . entity)

catIDFun :: Database.EntityFilterParam -> ArticleListEndo
catIDFun (Database.EFPIntOptional (Just i)) = filter ((== ID i) . entityID . category . entity)

tagIDFun :: Database.EntityFilterParam -> ArticleListEndo
tagIDFun (Database.EFPIntOptional (Just i)) = filter ((ID i `elem`) . map entityID . tags . entity)

tagIDInFun :: Database.EntityFilterParam -> ArticleListEndo
tagIDInFun (Database.EFPIntListOptional (Just (map ID -> tagIn))) =
  filter ((any (`elem` tagIn)) . map entityID . tags . entity)

tagIDAllFun :: Database.EntityFilterParam -> ArticleListEndo
tagIDAllFun (Database.EFPIntListOptional (Just (map ID -> tagAll))) =
  filter ((all (`elem` tagAll)) . map entityID . tags . entity)

arTitleFun :: Database.EntityFilterParam -> ArticleListEndo
arTitleFun (Database.EFPTextOptional (Just t)) = filter ((== t) . title . entity)

arContentFun :: Database.EntityFilterParam -> ArticleListEndo
arContentFun (Database.EFPTextOptional (Just t)) = filter ((t `T.isInfixOf`) . content . entity)

substrFun :: Database.EntityFilterParam -> ArticleListEndo
substrFun (Database.EFPTextOptional (Just t)) = filter (isSubstr t . entity)
  where
    isSubstr s Article {..} =
      let l = login $ entity $ user $ entity $ author
          c = unCatName $ name $ entity $ category
          cs = map unCatName $ parent $ entity category
          ts = map (tag . entity) tags
       in any (t `T.isInfixOf`) ([content, l, c] <> cs <> ts)

limitFun :: Database.EntityFilterParam -> ArticleListEndo
limitFun (Database.EFPInt i) = take i

offsetFun :: Database.EntityFilterParam -> ArticleListEndo
offsetFun (Database.EFPInt i) = drop i
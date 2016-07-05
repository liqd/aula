{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Frontend.Fragment.Category
    ( CategoryLabel(CategoryLabel)
    , CategoryMiniLabel(CategoryMiniLabel)
    , categoryFilterButtons
    , formPageSelectCategory
    , makeFormSelectCategory
    )
where

import Frontend.Prelude
import Frontend.Fragment.WhatListPage

import qualified Data.Text as ST
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Lucid.Html5 as DF


-- | FIXME: 'makeFormSelectCategory', 'formPageSelectCategory' should be a subform.  (related: `grep
-- subform src/Frontend/Page/Topic.hs`.)
-- FIXME: Error and non selected category are inseparable cases.
makeFormSelectCategory :: (Monad n, Monad m) => Maybe Category -> DF.Form (HtmlT n ()) m (Maybe Category)
makeFormSelectCategory mcat = formSelectorToCategory <$> DF.text (formSelectorFromCategory <$> mcat)

formSelectorToCategory :: ST -> Maybe Category
formSelectorToCategory (ST.commonPrefixes "select-.idea-category." -> Just (_, "", s))
                         = toEnumMay =<< readMay (cs s)
formSelectorToCategory _ = Nothing

formSelectorFromCategory :: Category -> ST
formSelectorFromCategory = ("select-.idea-category." <>) . cs . show . fromEnum


-- | see also: static/js/custom.js.
formPageSelectCategory :: Monad m => View (HtmlT m ()) -> HtmlT m ()
formPageSelectCategory v = do
    label_ $ do
        span_ [class_ "label-text"]
            "Kann deine Idee einer der folgenden Kategorieren zugeordnet werden?"
        DF.inputHidden "idea-category" v
        div_ [class_ "icon-list m-inline category-image-select"] $ do
            ul_ $ toHtml `mapM_` [(minBound :: CategorySelectButton)..]

newtype CategoryLabel = CategoryLabel Category
  deriving (Eq, Ord, Bounded, Enum, Show, Read, Generic)

instance ToHtml CategoryLabel where
    toHtmlRaw = toHtml
    toHtml (CategoryLabel cat) = toHtml $ CategorySelectButton cat
        -- FIXME: something without the `li_` elem?

newtype CategoryMiniLabel = CategoryMiniLabel Category
  deriving (Eq, Ord, Bounded, Enum, Show, Read, Generic)

instance ToHtml CategoryMiniLabel where
    toHtmlRaw = toHtml
    toHtml (CategoryMiniLabel cat) =
        li_ [class_ $ "icon-" <> toUrlPiece cat] . span_ $ uilabel cat

categoryFilterButtons :: Monad m => WhatListPage -> IdeasQuery -> HtmlT m ()
categoryFilterButtons whatListPage q = div_ [class_ "icon-list"] $ do
    p_ $ b_ "Filtere nach Kategorie"
    br_ []
    ul_ $ do
        for_ [minBound..] $ \cat -> do
            li_ [ class_ . ST.unwords $
                    ("icon-" <> toUrlPiece cat) : [ "m-active" | q ^. ideasQueryF == IdeasWithCat cat ]
                ] .
                a_ [href_ $ pathToIdeaListPage whatListPage (Just $ q & ideasQueryF %~ toggleIdeasFilter cat)] $
                    uilabel cat
        li_ [ class_ . ST.unwords $
                "icon-all-cats" : [ "m-active" | q ^. ideasQueryF == AllIdeas ]
            ] .
            a_ [href_ $ pathToIdeaListPage whatListPage (Just $ q & ideasQueryF .~ AllIdeas)] $
                "Alle Kategorien"


-- * local types

-- | only for selecting a category, not for filtering.  for the latter, see 'categoryFilterButtons'
-- below.  (it's a newtype so deriving is easier.)
newtype CategorySelectButton = CategorySelectButton Category
  deriving (Eq, Ord, Bounded, Enum, Show, Read, Generic)

-- | The "m-active" class is managed in js.  See `static/js/custom.js`.
instance ToHtml CategorySelectButton where
    toHtmlRaw = toHtml
    toHtml (CategorySelectButton cat) = li_ [class_ $ "icon-" <> toUrlPiece cat] .
        span_ [ class_ "icon-list-button"
              , id_ $ "select-.idea-category." <> (cs . show $ fromEnum cat)
              ] $ uilabel cat

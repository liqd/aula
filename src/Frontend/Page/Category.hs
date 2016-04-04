{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Frontend.Page.Category
where

import Frontend.Prelude

import qualified Frontend.Path as U
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Lucid.Html5 as DF
import qualified Text.Digestive.Types as DF


-- | FIXME: 'makeFormSelectCategory', 'formPageSelectCategory' should be a subform.  (related: `grep
-- subform src/Frontend/Page/Topic.hs`.)
makeFormSelectCategory :: (Monad m) => DF.Form (Html ()) m Category
makeFormSelectCategory = DF.validate f $ DF.text Nothing
  where
    f :: ST -> DF.Result (Html ()) Category
    f = maybe (DF.Error "bad category identifier") DF.Success
      . (toEnumMay <=< readMay)
      . cs

formPageSelectCategory :: Monad m => View (HtmlT m ()) -> HtmlT m ()
formPageSelectCategory v = do
    label_ $ do
        span_ [class_ "label-text"]
            "Kann deine Idee einer der folgenden Kategorieren zugeordnet werden?"
        DF.inputHidden "idea-category" v
        div_ [class_ "icon-list m-inline category-image-select"] $ do
            ul_ $ toHtml `mapM_` [(minBound :: CategoryButton)..]
                -- FIXME: select a category for the newly created idea.  this
                -- needs to be tested.  see also: static/js/custom.js.


newtype CategoryButton = CategoryButton Category
  deriving (Eq, Ord, Bounded, Enum, Show, Read, Generic)

newtype CategoryLabel = CategoryLabel Category
  deriving (Eq, Ord, Bounded, Enum, Show, Read, Generic)

instance ToHtml CategoryLabel where
    toHtmlRaw = toHtml
    toHtml (CategoryLabel cat) = toHtml $ CategoryButton cat
        -- FIXME: something without the `li_` elem?

instance ToHtml CategoryButton where
    toHtmlRaw = toHtml
    toHtml (CategoryButton CatRule) = li_ [class_ "icon-rules"] $
        span_ [class_ "icon-list-button", id_ "select-.idea-category.0"] "Regeln"
    toHtml (CategoryButton CatEquipment) = li_ [class_ "icon-equipment"] $
        span_ [class_ "icon-list-button", id_ "select-.idea-category.1"] "Ausstattung"
    toHtml (CategoryButton CatClass) = li_ [class_ "icon-teaching"] $
        span_ [class_ "icon-list-button", id_ "select-.idea-category.2"] "Unterricht"
    toHtml (CategoryButton CatTime) = li_ [class_ "icon-time"] $
        span_ [class_ "icon-list-button", id_ "select-.idea-category.3"] "Zeit"
    toHtml (CategoryButton CatEnvironment) = li_ [class_ "icon-environment"] $
        span_ [class_ "icon-list-button", id_ "select-.idea-category.4"] "Umgebung"
      -- FIXME: do something with [minBound..] here.


categoryToValue :: IsString s => Category -> s
categoryToValue CatRule        = "Regel"
categoryToValue CatEquipment   = "Ausstattung"
categoryToValue CatClass       = "Unterricht"
categoryToValue CatTime        = "Zeit"
categoryToValue CatEnvironment = "Umgebung"

categoryValues :: IsString s => [(Category, s)]
categoryValues = (\c -> (c, categoryToValue c)) <$> [minBound..]


categoryFilterButtons :: Monad m => HtmlT m ()
categoryFilterButtons = div_ [class_ "icon-list"] $ do
    ul_ $ do
        -- FIXME: these buttons should filter the ideas by category
        -- FIXME: do something with [minBound..] here.
        li_ [class_ "icon-rules"] $ do
            a_ [href_ U.Broken] "Regeln"
        li_ [class_ "icon-equipment"] $ do
            a_ [href_ U.Broken] "Ausstattung"
        li_ [class_ "icon-teaching"] $ do
            a_ [href_ U.Broken] "Unterricht"
        li_ [class_ "icon-time"] $ do
            a_ [href_ U.Broken] "Zeit"
        li_ [class_ "icon-environment"] $ do
            a_ [href_ U.Broken] "Umgebung"

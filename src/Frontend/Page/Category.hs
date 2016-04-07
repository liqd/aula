{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DataKinds                  #-}
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
import qualified Lucid
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Lucid.Html5 as DF
import qualified Text.Digestive.Types as DF


type IdeasFilterApi = QueryParam "category" Category
type IdeasFilterQuery = Maybe Category

ideasFilterQuery :: IdeasFilterQuery -> [Idea] -> [Idea]
ideasFilterQuery = \case
    (Just cat) -> filter ((== cat) . view ideaCategory)
    Nothing    -> id


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
      -- TODO: do something with [minBound..] here.


-- TODO: rename to categoryTOUIString; move to Types; have all mappings to and from string in one
-- place.
categoryToValue :: IsString s => Category -> s
categoryToValue CatRule        = "Regel"
categoryToValue CatEquipment   = "Ausstattung"
categoryToValue CatClass       = "Unterricht"
categoryToValue CatTime        = "Zeit"
categoryToValue CatEnvironment = "Umgebung"

categoryValues :: IsString s => [(Category, s)]
categoryValues = (\c -> (c, categoryToValue c)) <$> [minBound..]


linkToCategory :: IdeaLocation -> Category -> ST
linkToCategory loc cat =
       (absoluteUriPath . relPath . U.listIdeas $ loc)
    <> "?category=" <> toUrlPiece cat

categoryFilterButtons :: Monad m => IdeaLocation -> HtmlT m ()
categoryFilterButtons loc = div_ [class_ "icon-list"] $ do
    ul_ . for_ [minBound..] $ \cat ->
        li_ [class_ $ "icon-" <> toUrlPiece cat] $
            a_ [Lucid.href_ $ linkToCategory loc cat] (categoryToValue cat)

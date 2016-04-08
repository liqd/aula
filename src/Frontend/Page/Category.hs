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
    ( CategoryLabel(CategoryLabel)
    , IdeasFilterApi
    , IdeasFilterQuery
    , categoryFilterButtons
    , categoryToUiText
    , categoryUiTexts
    , formPageSelectCategory
    , ideasFilterQuery
    , linkToCategory
    , makeFormSelectCategory
    )
where

import Frontend.Prelude

import qualified Data.Text as ST
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

-- | see also: static/js/custom.js.
formPageSelectCategory :: Monad m => View (HtmlT m ()) -> HtmlT m ()
formPageSelectCategory v = do
    label_ $ do
        span_ [class_ "label-text"]
            "Kann deine Idee einer der folgenden Kategorieren zugeordnet werden?"
        DF.inputHidden "idea-category" v
        div_ [class_ "icon-list m-inline category-image-select"] $ do
            ul_ $ toHtml `mapM_` [(minBound :: CategoryButton)..]

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
    toHtml (CategoryButton cat) = li_ [class_ $ "icon-" <> toUrlPiece cat] .
        span_ [ class_ "icon-list-button"
              , id_ $ "select-.idea-category." <> (cs . show $ fromEnum cat)
              ] $ categoryToUiText cat

categoryToUiText :: IsString s => Category -> s
categoryToUiText CatRules       = "Regeln"
categoryToUiText CatEquipment   = "Ausstattung"
categoryToUiText CatTeaching    = "Unterricht"
categoryToUiText CatTime        = "Zeit"
categoryToUiText CatEnvironment = "Umgebung"

categoryUiTexts :: IsString s => [(Category, s)]
categoryUiTexts = (\c -> (c, categoryToUiText c)) <$> [minBound..]


linkToCategory :: IdeaLocation -> Maybe Category -> ST
linkToCategory loc mcat =
       (absoluteUriPath . relPath . U.listIdeas $ loc)
    <> maybe nil (("?category=" <>) . toUrlPiece) mcat

categoryFilterButtons :: Monad m => IdeaLocation -> IdeasFilterQuery -> HtmlT m ()
categoryFilterButtons loc filterQuery = div_ [class_ "icon-list"] $ do
    ul_ . for_ [minBound..] $ \cat -> do
        li_ [ class_ . ST.unwords $
                ("icon-" <> toUrlPiece cat) :
                [ "m-active" | filterQuery == Just cat ]
            ] $
            let filterQuery' = if filterQuery == Just cat
                  then Nothing
                  else Just cat
            in a_ [Lucid.href_ $ linkToCategory loc filterQuery']
                (categoryToUiText cat)

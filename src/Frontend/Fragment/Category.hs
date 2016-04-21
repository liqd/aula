{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Frontend.Fragment.Category
    ( CategoryLabel(CategoryLabel)
    , categoryFilterButtons
    , categoryToUiText
    , categoryUiTexts
    , formPageSelectCategory
    , makeFormSelectCategory
    )
where

import Frontend.Prelude

import qualified Data.Text as ST
import qualified Lucid
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Lucid.Html5 as DF
import qualified Text.Digestive.Types as DF


-- | FIXME: 'makeFormSelectCategory', 'formPageSelectCategory' should be a subform.  (related: `grep
-- subform src/Frontend/Page/Topic.hs`.)
makeFormSelectCategory :: (Monad m) => Maybe Category -> DF.Form (Html ()) m (Maybe Category)
makeFormSelectCategory mcat = DF.validate f $ DF.text (cs . show . fromEnum <$> mcat)
  where
    f :: ST -> DF.Result (Html ()) (Maybe Category)
    f = DF.Success . (toEnumMay <=< readMay) . cs

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


categoryFilterButtons :: Monad m => IdeaLocation -> IdeasQuery -> HtmlT m ()
categoryFilterButtons loc (IdeasQuery qf qs) = div_ [class_ "icon-list"] $ do
    ul_ . for_ [minBound..] $ \cat -> do
        li_ [ class_ . ST.unwords $
                ("icon-" <> toUrlPiece cat) :
                [ "m-active" | qf == Just cat ]
            ] $
            let qf' = if qf == Just cat
                  then Nothing
                  else Just cat
            in a_ [Lucid.href_ $ listIdeasWithQuery loc (IdeasQuery qf' qs)]
                (categoryToUiText cat)

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
    , formPageSelectCategory
    , makeFormSelectCategory
    )
where

import Debug.Trace

import Frontend.Prelude

import qualified Frontend.Path as U
import qualified Data.Text as ST
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Lucid.Html5 as DF


-- | FIXME: 'makeFormSelectCategory', 'formPageSelectCategory' should be a subform.  (related: `grep
-- subform src/Frontend/Page/Topic.hs`.)
-- FIXME: Error and non selected category are inseparable cases.
makeFormSelectCategory :: (Monad m) => Maybe Category -> DF.Form (Html ()) m (Maybe Category)
makeFormSelectCategory mcat = formSelectorToCategory <$> DF.text (cs . show . fromEnum <$> mcat)

formSelectorToCategory :: ST -> Maybe Category
formSelectorToCategory = (toEnumMay <=< readMay) . cs

-- | see also: static/js/custom.js.
formPageSelectCategory :: Monad m => View (HtmlT m ()) -> HtmlT m ()
formPageSelectCategory v = do
    label_ $ do
        span_ [class_ "label-text"]
            "Kann deine Idee einer der folgenden Kategorieren zugeordnet werden?"
        DF.inputHidden "idea-category" v
        div_ [class_ "icon-list m-inline category-image-select"] $ do
            ul_ $ toHtml `mapM_` [(minBound :: CategoryButton)..]

-- | only for selecting a category, not for filtering.  for the latter, see 'categoryFilterButtons'
-- below.  (it's a newtype so deriving is easier.)
newtype CategoryButton = CategoryButton Category
  deriving (Eq, Ord, Bounded, Enum, Show, Read, Generic)

newtype CategoryLabel = CategoryLabel Category
  deriving (Eq, Ord, Bounded, Enum, Show, Read, Generic)

instance ToHtml CategoryLabel where
    toHtmlRaw = toHtml
    toHtml (CategoryLabel cat) = toHtml $ CategoryButton cat
        -- FIXME: something without the `li_` elem?

-- | The "m-active" class is managed in js.  See `static/js/custom.js`.
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


categoryFilterButtons :: Monad m => Maybe ListIdeasInTopicTab -> IdeaLocation -> IdeasQuery -> HtmlT m ()
categoryFilterButtons mtab loc q = div_ [class_ "icon-list"] $ do
    ul_ . for_ [minBound..] $ \cat -> do
        traceShow ("...>>" :: String) $ pure ()
        traceShow ("...>>" <> show (("icon-" <> toUrlPiece cat) : [ "m-active" | q ^. ideasQueryF == IdeasWithCat cat ])) $ pure ()
-- "...>>[\"icon-rules\"]"
        traceShow ("...>>l" <> show loc) $ pure ()
-- "...>>lIdeaLocationSpace {_ideaLocationSpace = ClassSpace {_ideaSpaceSchoolClass = SchoolClass {_classSchoolYear = 2016, _className = \"5c\"}}}"
        traceShow ("...>>m" <> show mtab) $ pure ()
-- "...>>mJust ListIdeasInTopicTabAll"
        traceShow ("...>>t" <> show (q ^. ideasQueryF)) $ pure ()
-- "...>>tIdeasWithCat {_catFilter = CatEquipment}"
        traceShow ("...>>t" <> show q) $ pure ()
-- "...>>tIdeasQuery {_ideasQueryF = AllIdeas, _ideasQueryS = SortIdeasByTime}"
        traceShow ("...>>q" <> show (q & ideasQueryF %~ toggleIdeasFilter cat)) $ pure ()
-- "...>>qIdeasQuery {_ideasQueryF = IdeasWithCat {_catFilter = CatRules}, _ideasQueryS = SortIdeasByTime}"
        traceShow ("...>>u" <> show (U.listIdeas' loc mtab (Just $ q & ideasQueryF %~ toggleIdeasFilter cat))) $ pure ()
-- <interactive>: out of memory (requested 2097152 bytes)


        traceShow ("...>>" <> categoryToUiText cat :: String) $ pure ()
        li_ [ class_ . ST.unwords $
                ("icon-" <> toUrlPiece cat) : [ "m-active" | q ^. ideasQueryF == IdeasWithCat cat ]
            ] $ do
            traceShow ("...>>" :: String) $ pure ()
            a_ [href_ $ U.listIdeas' loc mtab (Just $ q & ideasQueryF %~ toggleIdeasFilter cat)]
                (categoryToUiText cat)
        traceShow ("...<<" :: String) $ pure ()

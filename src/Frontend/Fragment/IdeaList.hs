{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Fragment.IdeaList
    ( WhatListPage(..)
    , ListItemIdeas(..)
    , listItemIdeasCtx
    , listItemIdeasWhatPage
    , listItemIdeasLocation
    , listItemIdeasFilter
    , listItemIdeasData
    )
where

import Control.Lens

import Types
import LifeCycle (CapCtx)
import Frontend.Prelude
import Frontend.Fragment.Category
import Frontend.Fragment.VotesBar
import Persistent (IdeaStats(IdeaStats), ideaReachedQuorum)

import qualified Frontend.Path as U
import qualified Generics.SOP as SOP

data WhatListPage
    = IdeaInIdeasOverview
    | IdeaInViewTopic { _whatListPageTopicTab :: !ListIdeasInTopicTab }
    | IdeaInUserProfile
  deriving (Eq, Show, Read, Generic)

makeLenses ''WhatListPage
makePrisms ''WhatListPage

-- | One entry in an idea list.  Constructed from the 'IdeaStats' values in 'ListItemIdeas'.
data ListItemIdea = ListItemIdea
      { _listItemIdeaCtx        :: !CapCtx
      , _listItemIdeaWhatPage   :: !WhatListPage
      , _listItemIdeaInfo       :: !IdeaStats
      }
  deriving (Eq, Show, Read, Generic)

-- | An idea list.  Contains the information for constructing 'ListItemIdea' values, plus the
-- search/filter header of the list.
data ListItemIdeas = ListItemIdeas
      { _listItemIdeasCtx      :: !CapCtx
      , _listItemIdeasWhatPage :: WhatListPage -- TODO!
      , _listItemIdeasLocation :: IdeaLocation -- TODO!
      , _listItemIdeasFilter   :: !IdeasQuery
      , _listItemIdeasData     :: [IdeaStats] -- TODO!
      }
  deriving (Eq, Show, Read, Generic)

makeLenses ''ListItemIdeas

instance SOP.Generic WhatListPage
instance SOP.Generic ListItemIdea
instance SOP.Generic ListItemIdeas


instance ToHtml ListItemIdea where
    toHtmlRaw = toHtml
    toHtml p@(ListItemIdea ctx _whatListPage stats@(IdeaStats idea _phase _quo _voters)) = semanticDiv p $ do
        div_ [class_ "ideas-list-item"] $ do
            a_ [href_ $ U.viewIdea idea] $ do
                div_ [class_ "col-5-12"] $ do
                    div_ [class_ "ideas-list-img-container"] $ avatarImgFromHasMeta idea
                    div_ [class_ "ideas-list-text-container"] $ do
                        h2_ [class_ "ideas-list-title"] $ do
                            idea ^. ideaTitle . html
                            span_ [class_ "ideas-list-author"] $ do
                                "von " <> idea ^. (ideaMeta . metaCreatedByLogin) . unUserLogin . html

                div_ [class_ "col-3-12 ideas-list-indicator-container"] $ do
                    div_ [class_ "icon-list indicator-item m-inline m-display-only"] $ do
                        ul_ $ idea ^. ideaCategory . _Just . to CategoryMiniLabel . html

                    let notfeasible = do
                            div_ [class_ "indicator-item indicator-item-feasability is-not-feasible", title_ "not feasible"] $ do
                                div_ [class_ "indicator-icon"] "nicht durchführbar"

                        feasible = do
                            div_ [class_ "indicator-item indicator-item-feasability is-feasible", title_ "feasible"] $ do
                                div_ [class_ "indicator-icon"] "durchführbar"

                        readyfortable = do
                            div_ [class_ "m-table indicator-item"] $ do
                                  div_ [class_ "indicator-icon"] "Kann auf den Tisch"

                    case idea ^? ideaJuryResult . _Just . ideaJuryResultValue . to ideaJuryResultValueToType of
                        Nothing              -> nil  -- (not judged)
                        Just IdeaNotFeasible -> notfeasible
                        Just IdeaFeasible    -> feasible

                    when (ideaReachedQuorum stats && isWild (idea ^. ideaLocation))
                        readyfortable

                div_ [class_ "col-4-12 ideas-list-meta-container"] $ do
                    ul_ [class_ "meta-list"] $ do
                        li_ [class_ "meta-list-item"] $ do
                            i_ [class_ "meta-list-icon icon-comment-o"] nil
                            let s = idea ^. ideaComments . commentsCount
                            s ^. showed . html
                            if s == 1 then " Verbesserungsvorschlag" else " Verbesserungsvorschläge"

                    toHtml $ IdeaVoteLikeBars IdeaVoteLikeBarsPlain ctx stats

instance ToHtml ListItemIdeas where
    toHtmlRaw = toHtml
    toHtml p@(ListItemIdeas _ctx whatPage loc ideasQuery []) = semanticDiv p $ do
        ideaListHeader whatPage loc ideasQuery
        div_ [class_ "container-not-found"] . toHtml $ "Keine Ideen" <> mCatInfo <> "."
      where
        mCatInfo =
            ideasQuery ^. ideasQueryF . _IdeasWithCat . uilabeledST . to (" in der Kategorie " <>)

    toHtml p@(ListItemIdeas ctx whatPage loc ideasQuery ideasAndNumVoters) = semanticDiv p $ do
        ideaListHeader whatPage loc ideasQuery
        for_ ideasAndNumVoters $ toHtml . ListItemIdea ctx whatPage


-- | FUTUREWORK: there are no queries for IdeaInUserProfile.  to implement that, we need to refactor
-- the idea location that is currently used to calculate the urls for the filter and sort links.
ideaListHeader :: Monad m => WhatListPage -> IdeaLocation -> IdeasQuery -> HtmlT m ()
ideaListHeader IdeaInUserProfile _ _ = nil
ideaListHeader whatListPage loc ideasQuery = do
    let mtab' = whatListPage ^? whatListPageTopicTab
    categoryFilterButtons mtab' loc ideasQuery

    div_ [class_ "clearfix"] $ do
        div_ [class_ "btn-settings pop-menu"] $ do
            i_ [class_ "icon-sort", title_ "Sortieren nach"] nil
            ul_ [class_ "pop-menu-list"] $ do
                sequence_
                    [ let mactive | by == ideasQuery ^. ideasQueryS = " m-active"
                                  | otherwise                       = nil
                          hrf = href_ $ U.listIdeas' loc mtab' (Just $ ideasQuery & ideasQueryS .~ by)
                          txt = uilabel by
                      in li_ [class_ $ "pop-menu-list-item" <> mactive] $ a_ [hrf] txt
                    | by <- [minBound..] ]

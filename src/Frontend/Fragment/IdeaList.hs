{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Fragment.IdeaList
    ( module Frontend.Fragment.WhatListPage
    , ListItemIdeas(..)
    , listItemIdeasCtx
    , listItemIdeasWhatPage
    , listItemIdeasFilter
    , listItemIdeasData
    )
where

import Control.Lens

import Access (CapCtx, Capability(CanCreateIdea), capabilities, capCtxUser)
import Types
import Frontend.Prelude
import Frontend.Fragment.Category
import Frontend.Fragment.VotesBar
import Frontend.Fragment.WhatListPage
import Persistent (IdeaStats(IdeaStats), ideaReachedQuorum)

import qualified Frontend.Path as U
import qualified Generics.SOP as SOP


-- | One entry in an idea list.  Constructed from the 'IdeaStats' values in 'ListItemIdeas'.
data ListItemIdea = ListItemIdea
      { _listItemIdeaCtx        :: CapCtx
      , _listItemIdeaWhatPage   :: WhatListPage
      , _listItemIdeaInfo       :: IdeaStats
      }
  deriving (Eq, Show, Read, Generic)

-- | An idea list.  Contains the information for constructing 'ListItemIdea' values, plus the
-- search/filter header of the list.
data ListItemIdeas = ListItemIdeas
      { _listItemIdeasCtx      :: CapCtx
      , _listItemIdeasWhatPage :: WhatListPage
      , _listItemIdeasFilter   :: IdeasQuery
      , _listItemIdeasData     :: [IdeaStats]
      }
  deriving (Eq, Show, Read, Generic)

makeLenses ''ListItemIdeas

instance SOP.Generic ListItemIdea
instance SOP.Generic ListItemIdeas


instance ToHtml ListItemIdea where
    toHtmlRaw = toHtml
    toHtml p@(ListItemIdea _ctx _whatListPage stats@(IdeaStats idea _phase _quo _voters)) =
        semanticDiv' [class_ "ideas-list-item"] p $ do
            a_ [href_ $ U.viewIdea idea] $ do
                div_ [class_ "col-5-12"] $ do
                    div_ [class_ "ideas-list-img-container"] $
                        userAvatarImg' avatarDefaultSize (idea ^. createdBy) (idea ^. createdByLogin)
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

                    toHtml $ IdeaVoteLikeBars stats

instance ToHtml ListItemIdeas where
    toHtmlRaw = toHtml
    toHtml p@(ListItemIdeas ctx whatPage ideasQuery ideasAndNumVoters) = semanticDiv p $ do
        ideaListHeader whatPage ideasQuery
        callToActionOnList'
            (do
                "Keine Ideen" <> mCatInfo <> ".  "
                when canCreateIdea $
                    a_ [href_ createIdeaLink] "Erstelle Deine eigene Idee!")
            (toHtml . ListItemIdea ctx whatPage)
            ideasAndNumVoters
      where
        -- FIXME: Currently only students can create ideas,
        -- the check should be done in Access module, but it would
        -- introduce another hiearchical capability.
        caps = capabilities ctx
        canCreateIdea = case whatPage of
            IdeaInIdeasOverview _loc  -> CanCreateIdea `elem` caps
            IdeaInViewTopic _tab _loc -> CanCreateIdea `elem` caps
            IdeaInUserProfile _usr    -> ctx ^. capCtxUser . to isStudent

        mCatInfo =
            ideasQuery ^. ideasQueryF . _IdeasWithCat . uilabeled . to (" in der Kategorie " <>)

        createIdeaLink = case whatPage of
            IdeaInIdeasOverview loc  -> U.createIdea loc
            IdeaInViewTopic _tab loc -> U.createIdea loc
            IdeaInUserProfile _usr   -> U.createIdea (IdeaLocationSpace SchoolSpace)


ideaListHeader :: Monad m => WhatListPage -> IdeasQuery -> HtmlT m ()
ideaListHeader whatListPage ideasQuery = do

    categoryFilterButtons whatListPage ideasQuery

    div_ [class_ "clearfix"] $ do
        div_ [class_ "btn-settings pop-menu"] $ do
            i_ [class_ "icon-sort", title_ "Sortieren nach"] nil
            ul_ [class_ "pop-menu-list"] $ do
                sequence_
                    [ let mactive | by == ideasQuery ^. ideasQueryS = " m-active"
                                  | otherwise                       = nil
                          hrf = href_ $ pathToIdeaListPage whatListPage (Just $ ideasQuery & ideasQueryS .~ by)
                          txt = uilabel by
                      in li_ [class_ $ "pop-menu-list-item" <> mactive] $ a_ [hrf] txt
                    | by <- [minBound..] ]

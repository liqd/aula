{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Fragment.IdeaList
where

import Frontend.Prelude
import Frontend.Fragment.Category
import Frontend.Fragment.Feasibility
import Frontend.Fragment.QuorumBar
import LifeCycle

import qualified Frontend.Path as U
import qualified Generics.SOP as SOP
import qualified Lucid


data WhatListPage
    = IdeaInIdeasOverview
    | IdeaInViewTopic
    | IdeaInUserProfile
  deriving (Eq, Show, Read, Generic)

data ListItemIdea = ListItemIdea
      { _listItemRenderContext  :: RenderContext
      , _listItemIdeaWhatPage   :: WhatListPage
      , _listItemIdeaInfo       :: ListInfoForIdea
      }
  deriving (Eq, Show, Read, Generic)

data ListItemIdeas = ListItemIdeas
      { _listItemIdeasCtx      :: RenderContext
      , _listItemIdeasWhatPage :: WhatListPage
      , _listItemIdeasLocation :: IdeaLocation
      , _listItemIdeasFilter   :: IdeasQuery
      , _listItemIdeasData     :: [ListInfoForIdea]
      }
  deriving (Eq, Show, Read, Generic)

instance SOP.Generic WhatListPage
instance SOP.Generic ListItemIdea
instance SOP.Generic ListItemIdeas


instance ToHtml ListItemIdea where
    toHtmlRaw = toHtml
    toHtml p@(ListItemIdea ctx whatListPage (ListInfoForIdea idea mphase quo _voters)) = semanticDiv p $ do
        div_ [class_ "ideas-list-item"] $ do
            let caps = ideaCapabilities
                        (ctx ^. renderContextUser . _Id)
                        (ctx ^. renderContextUser . userRole)
                        idea
                        mphase

            when (IdeaInViewTopic == whatListPage) $ do
                feasibilityVerdict False idea caps

            a_ [href_ $ U.viewIdea idea Nothing] $ do
                -- FIXME use the phase
                div_ [class_ "col-8-12"] $ do
                    div_ [class_ "ideas-list-img-container"] $ avatarImgFromHasMeta idea
                    div_ [class_ "ideas-list-text-container"] $ do
                        h2_ [class_ "ideas-list-title"] $ do
                            idea ^. ideaTitle . html
                            span_ [class_ "ideas-list-author"] $ do
                                "von " <> idea ^. (ideaMeta . metaCreatedByLogin) . unUserLogin . html
                div_ [class_ "col-4-12 ideas-list-meta-container"] $ do
                    ul_ [class_ "meta-list"] $ do
                        li_ [class_ "meta-list-item"] $ do
                            i_ [class_ "meta-list-icon icon-comment-o"] nil
                            let s = idea ^. ideaComments . commentsCount
                            s ^. showed . html
                            if s == 1 then " Verbesserungsvorschlag" else " Verbesserungsvorschläge"
                        li_ [class_ "meta-list-item"] $ do
                            i_ [class_ "meta-list-icon icon-voting"] nil
                            toHtml (show (numLikes idea) <> " von " <> show quo <> " Quorum-Stimmen")
                    toHtml $ QuorumBar (percentLikes idea quo)

instance ToHtml ListItemIdeas where
    toHtmlRaw = toHtml
    toHtml p@(ListItemIdeas _ctx whatPage loc ideasQuery []) = semanticDiv p $ do
        ideaListHeader whatPage loc ideasQuery
        div_ [class_ "container-not-found"] . toHtml $ "Keine Ideen" <> fromMaybe nil mCatInfo <> "."
      where
        mCatInfo :: Maybe ST
        mCatInfo = (" in der Kategorie " <>) . categoryToUiText <$> (ideasQuery ^. ideasQueryF)

    toHtml p@(ListItemIdeas ctx whatPage loc ideasQuery ideasAndNumVoters) = semanticDiv p $ do
        ideaListHeader whatPage loc ideasQuery
        for_ ideasAndNumVoters $ toHtml . ListItemIdea ctx whatPage


-- | FUTUREWORK: there are no queries for IdeaInUserProfile.  to implement that, we need to refactor
-- the idea location that is currently used to calculate the urls for the filter and sort links.
ideaListHeader :: Monad m => WhatListPage -> IdeaLocation -> IdeasQuery -> HtmlT m ()
ideaListHeader IdeaInUserProfile _ _ = nil
ideaListHeader _ loc ideasQuery = do
    categoryFilterButtons loc ideasQuery

    div_ [class_ "clearfix"] $ do
        div_ [class_ "btn-settings pop-menu"] $ do
            i_ [class_ "icon-sort", title_ "Sortieren nach"] nil
            ul_ [class_ "pop-menu-list"] $ do
                let mk by text = do
                        li_ [class_ "pop-menu-list-item"] $
                            a_ [Lucid.href_ $ listIdeasWithQuery loc (ideasQuery & ideasQueryS .~ Just by)] text

                mk SortIdeasBySupport "Unterstützung"
                mk SortIdeasByAge     "Datum"

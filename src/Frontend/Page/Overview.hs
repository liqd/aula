{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Page.Overview
    ( PageRoomsOverview(..)
    , PageIdeasOverview(..)
    , PageIdeasInDiscussion(..)
    , WhatListPage(..)
    , viewRooms
    , viewIdeas
    , viewTopics
    )
where

import Action
import Frontend.Fragment.IdeaList
import Frontend.Fragment.QuorumBar ()
import Frontend.Prelude

import qualified Data.Text as ST
import qualified Frontend.Path as U


-- * pages

-- | 1. Rooms overview
data PageRoomsOverview = PageRoomsOverview [IdeaSpace]
  deriving (Eq, Show, Read)

-- | 2. Ideas overview
data PageIdeasOverview = PageIdeasOverview RenderContext IdeaSpace ListItemIdeas
  deriving (Eq, Show, Read)

-- | 3. Ideas in discussion (Topics overview)
data PageIdeasInDiscussion = PageIdeasInDiscussion IdeaSpace [Topic]
  deriving (Eq, Show, Read)

data Tabs = Tabs ActiveTab IdeaSpace
  deriving (Eq, Show, Read)

data ActiveTab = WildIdeas | Topics
  deriving (Eq, Show, Read)


-- * actions

viewRooms :: (ActionPersist m, ActionUserHandler m) => m PageRoomsOverview
viewRooms = PageRoomsOverview <$> getSpacesForCurrentUser

viewIdeas :: (ActionPersist m, ActionUserHandler m)
    => IdeaSpace -> IdeasQuery -> m PageIdeasOverview
viewIdeas space ideasQuery = do
    ctx <- renderContext
    PageIdeasOverview ctx space <$> equery (do
        is  <- applyFilter ideasQuery <$> findWildIdeasBySpace space
        ListItemIdeas ctx IdeaInIdeasOverview (IdeaLocationSpace space) ideasQuery
            <$> getListInfoForIdea `mapM` is)

viewTopics :: ActionPersist m => IdeaSpace -> m PageIdeasInDiscussion
viewTopics space = PageIdeasInDiscussion space <$> query (findTopicsBySpace space)


-- * templates

instance ToHtml PageRoomsOverview where
    toHtmlRaw = toHtml
    toHtml p@(PageRoomsOverview spaces) = semanticDiv p $ do
        div_ [class_ "container-main"] $ do
            f spaces
      where
        f :: forall m. (Monad m) => [IdeaSpace] -> HtmlT m ()
        f []       = p_ "Keine Ideenräume"
        f rs@(_:_) = forM_ rs g

        g :: forall m. (Monad m) => IdeaSpace -> HtmlT m ()
        g ispace = div_ [class_ "col-1-3"] $ do
            div_ [class_ ("item-room is-" <> showIdeaSpaceCategory ispace)] $ do
                a_ [href_ $ U.listIdeas (IdeaLocationSpace ispace)] $ do
                    span_ [class_ "item-room-image"] nil
                    h2_ [class_ "item-room-title"] $ uilabel ispace

instance Page PageRoomsOverview

instance ToHtml PageIdeasOverview where
    toHtmlRaw = toHtml
    toHtml p@(PageIdeasOverview _ctx space ideasAndNumVoters) = semanticDiv p $ do
        toHtml $ Tabs WildIdeas space
        header_ [class_ "ideas-header"] $ do
            h1_ [class_ "main-heading"] $ do
                span_ [class_ "sub-heading"] "Wilde ideen"
                "Was soll sich verändern?"
            p_ [class_ "sub-header"] . span_ $
                "Du kannst hier jede lose Idee, die du im Kopf hast, einwerfen und kannst für " <>
                "die Idee abstimmen und diese somit \"auf den Tisch bringen\"."
            button_ [onclick_ (U.createIdea (IdeaLocationSpace space)), class_ "btn-cta"] "+ Neue Idee"
        div_ [class_ "m-shadow"] $ do
            div_ [class_ "ideas-list"] $ toHtml ideasAndNumVoters

instance Page PageIdeasOverview where
    extraBodyClasses _ = ["m-shadow"]

instance ToHtml PageIdeasInDiscussion where
    toHtmlRaw = toHtml
    toHtml p@(PageIdeasInDiscussion space topics) = semanticDiv p $ do
        toHtml $ Tabs Topics space

        div_ [class_ "theme-grid"] $ do

            header_ [class_ "themes-header"] $ do
                -- WARNING: This button is not in the design. But it should be here for
                -- user experience reasons.
                -- FIXME: This button should de displayed only for Teachers.
                button_ [onclick_ (U.Space space U.CreateTopic), class_ "btn-cta"] "+ Neues Thema"

            forM_ topics $ \topic -> do
                div_ [class_ "col-1-3 theme-grid-col"] $ do
                    div_ [class_ ("theme-grid-item phase-" <> cs (show (topic ^. topicPhase)))] $ do
                        a_ [ class_ "theme-grid-item-link"
                           , href_ . U.listIdeas $ IdeaLocationTopic space (topic ^. _Id)
                           ] $ do
                            img_ [ src_ . U.static $ "images" </> case topic ^. topicPhase of
                                      PhaseWildIdea     -> "theme_aus.png"  -- FIXME
                                      PhaseWildFrozen   -> "theme_aus.png"  -- FIXME
                                      PhaseRefinement{} -> "theme_aus.png"
                                      PhaseRefFrozen{}  -> "theme_aus.png"
                                      PhaseJury         -> "theme_pruf.png"
                                      PhaseVoting{}     -> "theme_abs.png"
                                      PhaseVotFrozen{}  -> "theme_abs.png"
                                      PhaseResult       -> "theme_ergf.png"
                                 , class_ "theme-grid-item-image"
                                 ]
                            div_ [class_ "theme-grid-item-text"] $ do
                                span_ [class_ "theme-grid-item-phase"] $
                                    topic ^. topicPhase . uilabeledST . html
                                h2_   [class_ "theme-grid-item-title"] $
                                    topic ^. topicTitle . html
                                div_  [class_ "theme-grid-item-blurb"] $
                                    topic ^. topicDesc  . html

                                span_ [class_ "theme-grid-item-link"]
                                    "view topic"

instance Page PageIdeasInDiscussion

instance ToHtml Tabs where
    toHtmlRaw = toHtml
    toHtml (Tabs activeTab space) = ul_ [class_ "tabs"] $ do
        li_ [class_ . ST.unwords $
             "tab-item tab-item-wild-ideas" : ["m-active" | activeTab == WildIdeas]] $ do
            a_ [href_ $ U.listIdeas loc] $ do
                "Wilde Ideen " >> toHtml (spaceDesc space)
        li_ [class_ . ST.unwords $
             "tab-item tab-item-topics" : ["m-active" | activeTab == Topics]] $ do
            a_ [href_ $ U.Space space U.ListTopics] $ do
                "Ideen auf dem Tisch " >> toHtml (spaceDesc space)
      where
        spaceDesc ispace = "der " <> uilabelST ispace
        loc              = IdeaLocationSpace space

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Page.Overview
    ( PageOverviewOfSpaces(..)
    , PageOverviewOfWildIdeas(..)
    , PageOverviewOfTopics(..)
    , WhatListPage(..)
    , viewRooms
    , viewIdeas
    , viewTopics
    )
where

import Access
import Action
import Frontend.Fragment.IdeaList
import Frontend.Prelude
import Persistent (findWildIdeasBySpace, getIdeaStats, findTopicsBySpace, dbFreeze)

import qualified Data.Text as ST
import qualified Frontend.Path as U


-- * pages

-- | 1. Rooms overview
data PageOverviewOfSpaces = PageOverviewOfSpaces [IdeaSpace]
  deriving (Eq, Show, Read)

-- | 2. Ideas overview
data PageOverviewOfWildIdeas = PageOverviewOfWildIdeas CapCtx IdeaSpace ListItemIdeas
  deriving (Eq, Show, Read)

-- | 3. Ideas in discussion (Topics overview)
data PageOverviewOfTopics = PageOverviewOfTopics CapCtx IdeaSpace [Topic]
  deriving (Eq, Show, Read)

data Tabs = Tabs ActiveTab IdeaSpace
  deriving (Eq, Show, Read)

data ActiveTab = WildIdeas | Topics
  deriving (Eq, Show, Read)


-- * actions

viewRooms :: (ActionPersist m, ActionUserHandler m) => m PageOverviewOfSpaces
viewRooms = PageOverviewOfSpaces . sort <$> getSpacesForCurrentUser

viewIdeas :: (ActionPersist m, ActionUserHandler m)
    => IdeaSpace -> IdeasQuery -> m PageOverviewOfWildIdeas
viewIdeas space ideasQuery = do
    mphase <- Just . PhaseWildIdea <$> query (view dbFreeze)
    ctx <- set capCtxPhase mphase <$> spaceCapCtx space
    PageOverviewOfWildIdeas ctx space <$> equery (do
        is <- applyFilter ideasQuery <$> (findWildIdeasBySpace space >>= mapM getIdeaStats)
        pure $ ListItemIdeas ctx (IdeaInIdeasOverview (IdeaLocationSpace space)) ideasQuery is)

viewTopics :: (ActionPersist m, ActionUserHandler m) => IdeaSpace -> m PageOverviewOfTopics
viewTopics space =
    PageOverviewOfTopics <$> spaceCapCtx space <*> pure space <*> query (findTopicsBySpace space)


-- * templates

instance ToHtml PageOverviewOfSpaces where
    toHtmlRaw = toHtml
    toHtml p@(PageOverviewOfSpaces spaces) = semanticDiv' [class_ "container-main grid-view"] p $
        callToActionOnList'
            (p_ [class_ "container-not-found"] "Keine Ideenräume")
            ideaSpaceBox
            spaces
      where
        ideaSpaceBox :: forall m. (Monad m) => IdeaSpace -> HtmlT m ()
        ideaSpaceBox ispace = div_ [class_ "col-1-3"] $ do
            div_ [class_ ("item-room is-" <> showIdeaSpaceKind ispace)] $ do
                a_ [href_ $ U.listIdeas (IdeaLocationSpace ispace)] $ do
                    span_ [class_ "item-room-image"] nil
                    h2_ [class_ "item-room-title"] $ uilabel ispace

instance Page PageOverviewOfSpaces where
    -- Any logged in user is authorized since viewRooms is already selecting the right view
    isAuthorized = userPage

instance ToHtml PageOverviewOfWildIdeas where
    toHtmlRaw = toHtml
    toHtml p@(PageOverviewOfWildIdeas ctx space ideasAndNumVoters) = semanticDiv p $ do
        toHtml $ Tabs WildIdeas space
        header_ [class_ "ideas-header"] $ do
            h1_ [class_ "main-heading"] $ do
                span_ [class_ "sub-heading"] "Wilde ideen"
                "Was soll sich verändern?"
            p_ [class_ "sub-header"] . span_ $
                "Du kannst hier jede lose Idee, die du im Kopf hast, einwerfen und kannst für " <>
                "die Idee abstimmen und diese somit \"auf den Tisch bringen\"."
            when (CanCreateIdea `elem` capabilities ctx) $
                button_ [onclick_ (U.createIdea (IdeaLocationSpace space)), class_ "btn-cta m-large"] "+ Neue Idee"
        div_ [class_ "m-shadow"] $ do
            div_ [class_ "ideas-list"] $ toHtml ideasAndNumVoters

instance Page PageOverviewOfWildIdeas where
    -- Any logged in user is authorized since findWildIdeasBySpace is already selecting the right
    -- view.  FIXME: does this mean that we can visit enemy idea spaces, only we can't see any ideas
    -- in them?
    isAuthorized = userPage
    extraBodyClasses _ = ["m-shadow"]

instance ToHtml PageOverviewOfTopics where
    toHtmlRaw = toHtml
    toHtml p@(PageOverviewOfTopics ctx space topics) = semanticDiv p $ do
        toHtml $ Tabs Topics space

        div_ [class_ "theme-grid"] $ do
            let caps = capabilities ctx
            header_ [class_ "themes-header"] $ do
                -- WARNING: This button is not in the design. But it should be here for
                -- user experience reasons.
                when (CanCreateTopic `elem` caps) $
                    button_ [onclick_ (U.createTopic space), class_ "btn-cta m-large"] "+ Neues Thema"

            callToActionOnList'
                (do
                    "Hier gibt es noch keine Themen.  "
                    when (CanCreateTopic `elem` caps) .
                        a_ [href_ $ U.createTopic space] $
                            "Lege das erste Thema an!")
                (\topic -> div_ [class_ "col-1-3 theme-grid-col"] $ do
                    div_ [class_ ("theme-grid-item phase-" <> cs (show (topic ^. topicPhase)))] $ do
                        a_ [ class_ "theme-grid-item-link"
                           , href_ $ U.listIdeas (IdeaLocationTopic space (topic ^. _Id))
                           ] $ do
                            img_ [ src_ . U.TopStatic $ "images" </> case topic ^. topicPhase of
                                      PhaseWildIdea{}   -> "theme_aus.png"  -- FIXME
                                      PhaseRefinement{} -> "theme_aus.png"
                                      PhaseJury         -> "theme_pruf.png"
                                      PhaseVoting{}     -> "theme_abs.png"
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
                                    "Thema anzeigen")
                topics

instance Page PageOverviewOfTopics where
    -- Any logged in user is authorized since findTopicsBySpace is already selecting the right view.
    -- FIXME: does this mean that we can visit enemy idea spaces, only we can't see any ideas in
    -- them?
    isAuthorized = userPage

instance ToHtml Tabs where
    toHtmlRaw = toHtml
    toHtml (Tabs activeTab space) = ul_ [class_ "tabs"] $ do
        li_ [class_ . ST.unwords $
             "tab-item tab-item-wild-ideas" : ["m-active" | activeTab == WildIdeas]] $ do
            a_ [href_ $ U.listIdeas loc] $ do
                "Wilde Ideen " >> toHtml (spaceDesc space)
        li_ [class_ . ST.unwords $
             "tab-item tab-item-topics" : ["m-active" | activeTab == Topics]] $ do
            a_ [href_ $ U.listTopics space] $ do
                "Ideen auf dem Tisch " >> toHtml (spaceDesc space)
      where
        spaceDesc ispace = "der " <> uilabelST ispace
        loc              = IdeaLocationSpace space

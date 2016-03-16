{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -Werror #-}

module Frontend.Page.Overview
    ( PageRoomsOverview(..)
    , PageIdeasOverview(..)
    , PageIdeasInDiscussion(..)
    , viewRooms
    , viewIdeas
    , viewTopics
    )
where

import Action
import Frontend.Prelude

import qualified Frontend.Path as U
import qualified Data.Text as ST


-- * pages

-- | 1. Rooms overview
data PageRoomsOverview = PageRoomsOverview [IdeaSpace]
  deriving (Eq, Show, Read)

-- | 2. Ideas overview
data PageIdeasOverview = PageIdeasOverview IdeaSpace [(Idea, Int)]
  deriving (Eq, Show, Read)

-- | 3. Ideas in discussion (Topics overview)
data PageIdeasInDiscussion = PageIdeasInDiscussion IdeaSpace [Topic]
  deriving (Eq, Show, Read)

data Tabs = Tabs ActiveTab IdeaSpace
  deriving (Eq, Show, Read)

data ActiveTab = WildIdeas | Topics
  deriving (Eq, Show, Read)


-- * actions

viewRooms :: (ActionPersist r m, ActionUserHandler m, MonadError ActionExcept m)
    => m (Frame PageRoomsOverview)
viewRooms = makeFrame =<< persistent (PageRoomsOverview <$> getSpaces)

viewIdeas :: (ActionPersist r m, ActionUserHandler m, MonadError ActionExcept m)
    => IdeaSpace -> m (Frame PageIdeasOverview)
viewIdeas space = makeFrame =<< persistent
    (PageIdeasOverview space <$> (findWildIdeasBySpace space >>= mapM getNumVotersForIdea))

viewTopics :: (ActionPersist r m, ActionUserHandler m, MonadError ActionExcept m)
    => IdeaSpace -> m (Frame PageIdeasInDiscussion)
viewTopics space = makeFrame =<< persistent (PageIdeasInDiscussion space <$> findTopicsBySpace space)


-- * templates

instance ToHtml PageRoomsOverview where
    toHtmlRaw = toHtml
    toHtml p@(PageRoomsOverview spaces) = semanticDiv p $ do
        div_ [class_ "grid container-main"] $ do
            f spaces
      where
        f :: forall m. (Monad m) => [IdeaSpace] -> HtmlT m ()
        f []       = p_ "Keine Ideenräume"
        f rs@(_:_) = forM_ rs g

        g :: forall m. (Monad m) => IdeaSpace -> HtmlT m ()
        g ispace = div_ [class_ "col-1-3"] $ do
            div_ [class_ ("item-room is-" <> showIdeaSpaceCategory ispace)] $ do
                a_ [href_ $ U.Space ispace U.ListIdeas] $ do
                    span_ [class_ "item-room-image"] nil
                    h2_ [class_ "item-room-title"] $ h ispace


        h SchoolSpace = "Schule"
        h (ClassSpace c) = "Klasse " <> c ^. className . html
            -- for the first school year, we can ignore the year.  (after that, we have different
            -- options.  one would be to only show the year if it is not the current one, or always show
            -- it, or either show "current" if applicable or the actual year if it lies in the past.)

instance Page PageRoomsOverview where
    isPrivatePage _ = True

instance ToHtml PageIdeasOverview where
    toHtmlRaw = toHtml
    toHtml p@(PageIdeasOverview space ideaAndNumVoters) = semanticDiv p $ do
        toHtml $ Tabs WildIdeas space
        header_ [class_ "ideas-header"] $ do
            h1_ [class_ "main-heading"] $ do
                span_ [class_ "sub-heading"] "WILDE IDEEN"
                "Was soll sich verändern?"
            p_ [class_ "sub-header"] . span_ $
                "Du kannst hier jede lose Idee, die du im Kopf hast, einwerfen und kannst für " <>
                "die Idee abstimmen und diese somit \"auf den Tisch bringen\"."
            button_ [onclick_ (U.Space space U.CreateIdea), class_ "btn-cta"] "+ Neue Idee"
        div_ [class_ "icon-list"] $ do
            ul_ $ do
                -- FIXME: these buttons should filter the ideas by category
                -- FIXME: also, there should be a way to generate these with something like @f `mapM_` [minBound..]@
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
        div_ [class_ "m-shadow"] $ do
            div_ [class_ "grid"] $ do
                div_ [class_ "ideas-list"] . for_ ideaAndNumVoters $ \(idea, numVoters) ->
                    ListItemIdea True Nothing numVoters idea ^. html

instance Page PageIdeasOverview where
    isPrivatePage _ = True

instance ToHtml PageIdeasInDiscussion where
    toHtmlRaw = toHtml
    toHtml p@(PageIdeasInDiscussion space topics) = semanticDiv p $ do
        toHtml $ Tabs Topics space

        -- WARNING: This button is not in the design. But it should be here for
        -- user experience reasons.
        -- FIXME: This button should de displayed only for Teachers.
        button_ [onclick_ (U.Space space U.CreateTopic), class_ "btn-cta"] "+ Neues Thema"

        forM_ topics $ \topic -> do
            hr_ []
            img_ [src_ $ U.TopStatic "FIXME", alt_ "FIXME"]
            div_ . toHtml . show $ topic ^. topicPhase
            div_ . toHtml $ topic ^. topicTitle
            div_ . toHtml $ topic ^. topicDesc
            a_ [href_ . U.Space space . U.ListTopicIdeas $ topic ^. _Id] "view topic"

instance Page PageIdeasInDiscussion where
    isPrivatePage _ = True

instance ToHtml Tabs where
    toHtmlRaw = toHtml
    toHtml (Tabs activeTab space) = ul_ [class_ "tabs"] $ do
        li_ [class_ . ST.unwords $
             "tab-item tab-item-wild-ideas" : ["m-active" | activeTab == WildIdeas]] $ do
            a_ [href_ $ U.Space space U.ListIdeas] $ do
                "Wilde Ideen " >> toHtml (spaceDesc space)
        li_ [class_ . ST.unwords $
             "tab-item tab-item-topics" : ["m-active" | activeTab == Topics]] $ do
            a_ [href_ $ U.Space space U.ListTopics] $ do
                "Ideen auf dem Tisch " >> toHtml (spaceDesc space)
      where
        spaceDesc :: IdeaSpace -> ST
        spaceDesc SchoolSpace    = "der Schule"
        spaceDesc (ClassSpace c) = "der Klasse " <> c ^. className

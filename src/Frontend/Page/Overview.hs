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
import Frontend.Path (Top(TopTesting), path)
import Frontend.Prelude


----------------------------------------------------------------------
-- pages

-- | 1. Rooms overview
data PageRoomsOverview = PageRoomsOverview [IdeaSpace]
  deriving (Eq, Show, Read)

-- | 2. Ideas overview
data PageIdeasOverview = PageIdeasOverview IdeaSpace [Idea]
  deriving (Eq, Show, Read)

-- | 3. Ideas in discussion (Topics overview)
data PageIdeasInDiscussion = PageIdeasInDiscussion IdeaSpace [Topic]
  deriving (Eq, Show, Read)

data Tabs = Tabs ActiveTab IdeaSpace
  deriving (Eq, Show, Read)

data ActiveTab = WildIdeas | Topics
  deriving (Eq, Show, Read)


----------------------------------------------------------------------
-- actions

viewRooms :: ActionPersist m => m (Frame PageRoomsOverview)
viewRooms = persistent $ Frame frameUserHack . PageRoomsOverview <$> getSpaces

viewIdeas :: ActionPersist m => IdeaSpace -> m (Frame PageIdeasOverview)
viewIdeas space = persistent $ Frame frameUserHack . PageIdeasOverview space <$> findWildIdeasBySpace space

viewTopics :: ActionPersist m => IdeaSpace -> m (Frame PageIdeasInDiscussion)
viewTopics space = persistent $
  Frame frameUserHack . PageIdeasInDiscussion space <$> findTopicsBySpace space


----------------------------------------------------------------------
-- templates

instance ToHtml PageRoomsOverview where
    toHtmlRaw = toHtml
    toHtml p@(PageRoomsOverview spaces) = semanticDiv p $ f spaces
      where
        f :: forall m. (Monad m) => [IdeaSpace] -> HtmlT m ()
        f []       = p_ "Keine Ideenräume"
        f rs@(_:_) = forM_ rs g

        g :: forall m. (Monad m) => IdeaSpace -> HtmlT m ()
        g SchoolSpace = div_ . p_ $ "Schule"
        g (ClassSpace (SchoolClass n _)) = div_ . p_ $ "Klasse " <> toHtml n
            -- for the first school year, we can ignore the year.  (after that, we have different
            -- options.  one would be to only show the year if it is not the current one, or always show
            -- it, or either show "current" if applicable or the actual year if it lies in the past.)

instance Page PageRoomsOverview where
    isPrivatePage _ = True

instance ToHtml PageIdeasOverview where
    toHtmlRaw = toHtml
    toHtml p@(PageIdeasOverview space ideas) = semanticDiv p $ do
        toHtml $ Tabs WildIdeas space
        p_ "WILDE IDEEN"
        h1_ "Was soll sich verändern?"
        p_ $ "Du kannst hier jede lose Idee, die du im Kopf hast, einwerfen und kannst fuer die "
            <> "Idee abstimmen und diese somit \"auf den Tisch bringen\"."
        div_ $ button_ [onclick_ ("location.href='" <> (path $ TopTesting "/ideas/create") <> "'")] "+ Neue Idee" -- FIXME: should link to idea creation form
        div_ $ do
            -- FIXME: these buttons should filter the ideas by category
            button_ "Regeln"
            button_ "Ausstattung"
            button_ "Unterricht"
            button_ "Zeit"
            button_ "Umgebung"
        div_ [id_ "ideas"] . for_ ideas $ \idea ->
            ListItemIdea Nothing idea ^. html

instance Page PageIdeasOverview where
    isPrivatePage _ = True

instance ToHtml PageIdeasInDiscussion where
    toHtmlRaw = toHtml
    toHtml p@(PageIdeasInDiscussion space topics) = semanticDiv p $ do
        toHtml $ Tabs Topics space
        forM_ topics $ \topic -> do
            hr_ []
            img_ [src_ "FIXME", alt_ "FIXME"]
            div_ . toHtml . show $ topic ^. topicPhase
            div_ . toHtml $ topic ^. topicTitle
            div_ . toHtml $ topic ^. topicDesc
            a_ [href_ "FIXME: link to topic details"] "link"

instance Page PageIdeasInDiscussion where
    isPrivatePage _ = True

instance ToHtml Tabs where
    toHtmlRaw = toHtml
    toHtml (Tabs activeTab space) = div_ $ do
        span_ [class_ "active" | activeTab == WildIdeas] $ do
            "Wilde Ideen " >> toHtml (spaceDesc space)
        span_ [class_ "active" | activeTab == Topics] $ do
            "Ideen auf dem Tisch " >> toHtml (spaceDesc space)
      where
        spaceDesc :: IdeaSpace -> ST
        spaceDesc SchoolSpace     = "der Schule"
        spaceDesc (ClassSpace (SchoolClass n _)) = "der Klasse " <> n

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
import Frontend.Page.Category
import Frontend.Prelude
import Persistent

import qualified Frontend.Path as U
import qualified Data.Text as ST


-- * pages

-- | 1. Rooms overview
data PageRoomsOverview = PageRoomsOverview [IdeaSpace]
  deriving (Eq, Show, Read)

-- | 2. Ideas overview
data PageIdeasOverview = PageIdeasOverview IdeaSpace IdeasFilterQuery [(Idea, Int)]
  deriving (Eq, Show, Read)

-- | 3. Ideas in discussion (Topics overview)
data PageIdeasInDiscussion = PageIdeasInDiscussion IdeaSpace [Topic]
  deriving (Eq, Show, Read)

data Tabs = Tabs ActiveTab IdeaSpace
  deriving (Eq, Show, Read)

data ActiveTab = WildIdeas | Topics
  deriving (Eq, Show, Read)


-- * actions

viewRooms :: (ActionPersist m, ActionUserHandler m, MonadError ActionExcept m)
    => m (Frame PageRoomsOverview)
viewRooms = makeFrame =<< (PageRoomsOverview <$> query getSpaces)

viewIdeas :: (ActionPersist m, ActionUserHandler m, MonadError ActionExcept m)
    => IdeaSpace -> IdeasFilterQuery -> m (Frame PageIdeasOverview)
viewIdeas space mcat = makeFrame =<<
    (PageIdeasOverview space mcat <$> query (do
        is  <- ideasFilterQuery mcat <$> findWildIdeasBySpace space
        ivs <- getNumVotersForIdea `mapM` is
        pure ivs))

viewTopics :: (ActionPersist m, ActionUserHandler m, MonadError ActionExcept m)
    => IdeaSpace -> m (Frame PageIdeasInDiscussion)
viewTopics space = makeFrame =<< (PageIdeasInDiscussion space <$> query (findTopicsBySpace space))


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
                    h2_ [class_ "item-room-title"] $ h ispace


        h SchoolSpace = "Schule"
        h (ClassSpace c) = "Klasse " <> c ^. className . html
            -- for the first school year, we can ignore the year.  (after that, we have different
            -- options.  one would be to only show the year if it is not the current one, or always show
            -- it, or either show "current" if applicable or the actual year if it lies in the past.)

instance Page PageRoomsOverview

instance ToHtml PageIdeasOverview where
    toHtmlRaw = toHtml
    toHtml p@(PageIdeasOverview space filterQuery ideaAndNumVoters) = semanticDiv p $ do
        toHtml $ Tabs WildIdeas space
        header_ [class_ "ideas-header"] $ do
            h1_ [class_ "main-heading"] $ do
                span_ [class_ "sub-heading"] "WILDE IDEEN"
                "Was soll sich verändern?"
            p_ [class_ "sub-header"] . span_ $
                "Du kannst hier jede lose Idee, die du im Kopf hast, einwerfen und kannst für " <>
                "die Idee abstimmen und diese somit \"auf den Tisch bringen\"."
            button_ [onclick_ (U.createIdea (IdeaLocationSpace space)), class_ "btn-cta"] "+ Neue Idee"
        categoryFilterButtons (IdeaLocationSpace space) filterQuery
        div_ [class_ "m-shadow"] $ do
            div_ [class_ "ideas-list"] . for_ ideaAndNumVoters $ \(idea, numVoters) ->
                ListItemIdea True Nothing numVoters idea ^. html

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
                            img_ [ src_ . U.TopStatic $ "images" </> case topic ^. topicPhase of
                                      PhaseRefinement _ -> "theme_aus.png"
                                      PhaseJury         -> "theme_pruf.png"
                                      PhaseVoting     _ -> "theme_abs.png"
                                      PhaseResult       -> "theme_ergf.png"
                                 , class_ "theme-grid-item-image"
                                 ]
                            div_ [class_ "theme-grid-item-text"] $ do
                                span_ [class_ "theme-grid-item-phase"] . toHtml . phaseName
                                    $ topic ^. topicPhase
                                h2_   [class_ "theme-grid-item-title"] . toHtml
                                    $ topic ^. topicTitle
                                div_  [class_ "theme-grid-item-blurb"] . toHtml
                                    $ topic ^. topicDesc
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
        spaceDesc :: IdeaSpace -> ST
        spaceDesc SchoolSpace    = "der Schule"
        spaceDesc (ClassSpace c) = "der Klasse " <> c ^. className
        loc = IdeaLocationSpace space

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -Werror #-}

module Frontend.Page.Overview
    ( PageRoomsOverview(..)
    , PageIdeasOverview(..)
    , PageIdeasInDiscussion(..)
    , ListItemIdeaContext(..), ListItemIdea(ListItemIdea), ListItemIdeas(ListItemIdeas)
    , viewRooms
    , viewIdeas
    , viewTopics
    )
where

import Action
import Frontend.Page.Category
import Frontend.Prelude
import GHC.Generics (Generic)
import LifeCycle
import Lucid.Missing (onclick_, script_, href_, src_, postButton_, nbsp)

import qualified Data.Map as Map
import qualified Data.Text as ST
import qualified Frontend.Path as U
import qualified Generics.SOP as SOP


-- * pages

-- | 1. Rooms overview
data PageRoomsOverview = PageRoomsOverview [IdeaSpace]
  deriving (Eq, Show, Read)

-- | 2. Ideas overview
data PageIdeasOverview = PageIdeasOverview RenderContext IdeaSpace IdeasFilterQuery ListItemIdeas
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
viewIdeas space mcat = do
    ctx <- renderContext
    makeFrame =<< (PageIdeasOverview ctx space mcat
                    <$> equery (do
        is  <- ideasFilterQuery mcat <$> findWildIdeasBySpace space
        ListItemIdeas ctx mcat <$> getListInfoForIdea `mapM` is))

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
    toHtml p@(PageIdeasOverview ctx space filterQuery ideas) = semanticDiv p $ do
        toHtml $ Tabs WildIdeas space
        header_ [class_ "ideas-header"] $ do
            h1_ [class_ "main-heading"] $ do
                span_ [class_ "sub-heading"] "Wilde ideen"
                "Was soll sich verändern?"
            p_ [class_ "sub-header"] . span_ $
                "Du kannst hier jede lose Idee, die du im Kopf hast, einwerfen und kannst für " <>
                "die Idee abstimmen und diese somit \"auf den Tisch bringen\"."
            button_ [onclick_ (U.createIdea (IdeaLocationSpace space)), class_ "btn-cta"] "+ Neue Idee"
        categoryFilterButtons (IdeaLocationSpace space) filterQuery
        div_ [class_ "m-shadow"] $ do
            div_ [class_ "ideas-list"] . toHtml $ ideas

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


-- * idea lists

data ListItemIdeaContext
    = IdeaInIdeasOverview
    | IdeaInViewTopic
    | IdeaInUserProfile
  deriving (Eq, Show, Read, Generic)

data ListItemIdea = ListItemIdea
      { _listItemIdeaContext    :: ListItemIdeaContext
      , _listItemIdeaPhase      :: Maybe Phase
      , _listItemIdeaNumVoters  :: Int
      , _listItemIdea           :: Idea
      , _listItemRenderContext  :: RenderContext
      }
  deriving (Eq, Show, Read, Generic)

data ListItemIdeas =
    ListItemIdeas
        { _ideasAndNumVotersCtx    :: RenderContext
        , _ideasAndNumVotersFilter :: IdeasFilterQuery
        , _ideasAndNumVotersData   :: [(Idea, Maybe Phase, Int)]
        }
  deriving (Eq, Show, Read, Generic)

instance SOP.Generic ListItemIdeaContext
instance SOP.Generic ListItemIdea
instance SOP.Generic ListItemIdeas


instance ToHtml ListItemIdea where
    toHtmlRaw = toHtml
    toHtml p@(ListItemIdea listItemIdeaContext phase numVoters idea ctx) = semanticDiv p $ do
        div_ [class_ "ideas-list-item"] $ do
            let caps = ideaCapabilities
                        (ctx ^. renderContextUser . _Id)
                        idea
                        phase
                        (ctx ^. renderContextUser . userRole)

            when (IdeaInViewTopic == listItemIdeaContext) $ do
                when (MarkFeasiblity `elem` caps) . div_ $ do
                    let explToHtml :: forall m. Monad m => Document -> HtmlT m ()
                        explToHtml (Markdown text) = do
                            p_ "Begründung:"
                            p_ $ toHtml text

                    case _ideaJuryResult idea of
                        Nothing -> do
                            div_ [class_ "admin-buttons"] $ do
                                button_ [ class_ "btn-cta m-valid"
                                        , onclick_ $ U.judgeIdea idea IdeaFeasible
                                        ] $ do
                                    i_ [class_ "icon-check"] nil
                                    "durchführbar"
                                button_ [ class_ "btn-cta m-invalid"
                                        , onclick_ $ U.judgeIdea idea IdeaNotFeasible
                                        ] $ do
                                    i_ [class_ "icon-times"] nil
                                    "nicht durchführbar"
                        Just (IdeaJuryResult _ (Feasible maybeExpl)) -> do
                            div_ [class_ "info-text m-realised"] $ do
                                h3_ [class_ "info-text-header"] "durchführbar"
                                case maybeExpl of
                                    Just expl -> explToHtml expl
                                    Nothing -> nil
                        Just (IdeaJuryResult _ (NotFeasible expl)) -> do
                            div_ [class_ "info-text m-unrealised"] $ do
                                h3_ [class_ "info-text-header"] "nicht durchführbar"
                                explToHtml expl

            a_ [href_ $ U.viewIdea idea] $ do
                -- FIXME use the phase
                div_ [class_ "col-8-12"] $ do
                    div_ [class_ "ideas-list-img-container"] $ avatarImgFromHasMeta idea
                    div_ [class_ "ideas-list-text-container"] $ do
                        h2_ [class_ "ideas-list-title"] $ do
                            idea ^. ideaTitle . html
                            span_ [class_ "ideas-list-author"] $ do
                                "von " <> idea ^. (ideaMeta . metaCreatedByLogin) . fromUserLogin . html
                div_ [class_ "col-4-12 ideas-list-meta-container"] $ do
                    ul_ [class_ "meta-list"] $ do
                        li_ [class_ "meta-list-item"] $ do
                            i_ [class_ "meta-list-icon icon-comment-o"] nil
                            let s = idea ^. ideaComments . commentsCount
                            s ^. showed . html
                            if s == 1 then " Verbesserungsvorschlag" else " Verbesserungsvorschläge"
                        li_ [class_ "meta-list-item"] $ do
                            i_ [class_ "meta-list-icon icon-voting"] nil
                            toHtml (show numLikes <> " von " <> show numVoters <> " Stimmen")
                    span_ [class_ "progress-bar"] $ do
                        span_ [ class_ "progress-bar-progress"
                              , style_ ("width: " <> cs (show percentLikes) <> "%")
                              ]
                            nil
      where
        numLikes :: Int
        numLikes = Map.size $ idea ^. ideaLikes

        -- div by zero is caught silently: if there are no voters, the quorum stays 0%.
        -- FIXME: we could assert that values are always between 0..100, but the inconsistent test
        -- data violates that invariant.
        percentLikes :: Int
        percentLikes = {- assert c -} v
          where
            -- c = v >= 0 && v <= 100
            v = if numVoters == 0
                  then 0
                  else (numLikes * 100) `div` numVoters


instance ToHtml ListItemIdeas where
    toHtmlRaw = toHtml
    toHtml p@(ListItemIdeas _ctx filterq []) = semanticDiv p $ do
        p_ . toHtml $ "Keine Ideen" <> fromMaybe nil mCatInfo <> "."
      where
        mCatInfo :: Maybe ST
        mCatInfo = (" in der Kategorie " <>) . categoryToUiText <$> filterq

    toHtml (ListItemIdeas ctx _filterq ideasAndNumVoters) = do
        for_ ideasAndNumVoters $ \(idea, mphase, numVoters) ->
            ListItemIdea
                IdeaInViewTopic
                mphase
                numVoters idea
                ctx ^. html

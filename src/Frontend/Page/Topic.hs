{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TemplateHaskell     #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Page.Topic
    ( ViewTopic(..)
    , ViewTopicTab(..)
    , CreateTopic(..)
    , EditTopic(..)
    , IdeasFilterApi
    , viewTopic, viewTopicPage
    , createTopic
    , editTopic )
where

import Action ( ActionM, ActionPersist(..), ActionUserHandler, ActionExcept
              , getCurrentTimestamp
              )
import Control.Exception (assert)
import Frontend.Page.Category
import Frontend.Page.Overview
import Frontend.Prelude hiding (moveIdeasToLocation, editTopic)

import qualified Action (createTopic)
import qualified Frontend.Path as U
import qualified Persistent.Api as Persistent (EditTopic(EditTopic))
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Lucid.Html5 as DF


-- * types

data ViewTopicTab
  = TabAllIdeas     { _viewTopicTabFilter :: IdeasFilterQuery }
  | TabVotingIdeas  { _viewTopicTabFilter :: IdeasFilterQuery }
  | TabWinningIdeas { _viewTopicTabFilter :: IdeasFilterQuery }
  | TabDelegation
  deriving (Eq, Ord, Show, Read)

makeLenses ''ViewTopicTab
makePrisms ''ViewTopicTab

-- | 4 Topic overview
-- * 4.1 Topic overview: Refinement phase
-- * 4.2 Topic overview: Jury (assessment) phase
-- * 4.3 Topic overview: Voting phase
-- * 4.4 Topic overview: Result phase
-- * 4.5 Topic overview: Delegations
data ViewTopic
  = ViewTopicIdeas RenderContext ViewTopicTab Topic ListItemIdeas
  | ViewTopicDelegations Topic [Delegation]
  deriving (Eq, Show, Read)

instance Page ViewTopic where
    extraBodyClasses _ = ["m-shadow"]

-- | 10.1 Create topic: Create topic
data CreateTopic = CreateTopic IdeaSpace [Idea] Timestamp
  deriving (Eq, Show, Read)

instance Page CreateTopic

-- | 10.2 Create topic: Move ideas to topic (Edit topic)
data EditTopic = EditTopic IdeaSpace Topic [Idea]
  deriving (Eq, Show, Read)

instance Page EditTopic


-- * templates

tabLink :: Monad m => Topic -> ViewTopicTab -> ViewTopicTab -> HtmlT m ()
tabLink topic curTab targetTab =
  case targetTab of
    TabAllIdeas     _ -> go "tab-ideas"       U.listTopicIdeas        "Alle Ideen"
    TabVotingIdeas  _ -> g' "tab-voting"      U.ViewTopicIdeasVoting  "Ideen in der Abstimmung"
    TabWinningIdeas _ -> g' "tab-winning"     U.ViewTopicIdeasWinning "Gewinner"
    TabDelegation     -> g' "tab-delegations" U.ViewTopicDelegations  "Beauftragen Stimmen"
  where
    space = topic ^. topicIdeaSpace
    go ident uri =
        a_ [ id_ ident
           , href_ $ uri topic
           , class_ $ tabSelected curTab targetTab
           ]
    g' ident uri = go ident (U.Space space . uri . view _Id)

-- FIXME: how do we display a topic in the finished phase?
-- Is this the same the result phase?
-- Maybe some buttons to hide?
instance ToHtml ViewTopic where
    toHtmlRaw = toHtml

    toHtml p@(ViewTopicDelegations topic delegations) = semanticDiv p $ do
        viewTopicHeaderDiv topic TabDelegation
        -- related: Frontend.Page.User.renderDelegations
        -- FIXME: implement!
        pre_ $ topic ^. showed . html
        pre_ $ delegations ^. showed . html

    toHtml p@(ViewTopicIdeas _ctx tab topic ideasAndNumVoters) = semanticDiv p $ do
        assert (tab /= TabDelegation) $ viewTopicHeaderDiv topic tab
        div_ [class_ "ideas-list"] $ do
            categoryFilterButtons (topicIdeaLocation topic) (tab ^? viewTopicTabFilter . _Just)
            div_ [class_ "btn-settings pop-menu"] $ do  -- not sure what settings are meant here?
                i_ [class_ "icon-sort", title_ "Sortieren nach"] nil
                ul_ [class_ "pop-menu-list"] $ do
                    li_ [class_ "pop-menu-list-item"] $ do
                        a_ [href_ U.Broken] "Unterstützung"  -- FIXME Dummy
                    li_ [class_ "pop-menu-list-item"] $ do
                        a_ [href_ U.Broken] "Datum"  -- FIXME Dummy
            toHtml ideasAndNumVoters


viewTopicHeaderDiv :: Monad m => Topic -> ViewTopicTab -> HtmlT m ()
viewTopicHeaderDiv topic tab = do
    div_ [class_ "topic-header"] $ do
        header_ [class_ "detail-header"] $ do
            a_ [class_ "btn m-back detail-header-back", href_ $ U.Space space U.ListTopics] "Zu Allen Themen"
            nav_ [class_ "pop-menu m-dots detail-header-menu"] $ do
                ul_ [class_ "pop-menu-list"] $ do
                    li_ [class_ "pop-menu-list-item"] $ do
                        a_ [id_ "edit-topic",  href_ . U.Space space $ U.MoveIdeasToTopic topicId] $ do
                            i_ [class_ "icon-pencil"] nil
                            "Thema bearbeiten"
        h1_   [class_ "main-heading"] $ do
            span_ [class_ "sub-heading"] . toHtml $ phaseName phase
            toHtml $ topic ^. topicTitle
        p_ [class_ "sub-header"] $ topic ^. topicDesc . html
        div_ [class_ "heroic-btn-group"] $ do
            let createIdeaButton = do
                    a_ [ class_ "btn-cta heroic-cta"
                       , href_ . U.createIdea $ IdeaLocationTopic space topicId
                       ]
                     "+ Neue Idee"
                delegateVoteButton = do
                    a_  [ class_ "btn-cta heroic-cta"
                        , href_ . U.Space space $ U.CreateTopicDelegation topicId
                        ] $ do
                      i_ [class_ "icon-bullhorn"] nil
                      "Stimme Beauftragen"

            case phase of
                PhaseRefinement _ -> createIdeaButton >> delegateVoteButton
                PhaseJury         -> delegateVoteButton
                PhaseVoting     _ -> delegateVoteButton
                PhaseResult       -> nil

        div_ [class_ "heroic-tabs"] $ do
            let t1 = tabLink topic tab (TabAllIdeas Nothing)
                t2 = tabLink topic tab (TabVotingIdeas Nothing)
                t3 = tabLink topic tab (TabWinningIdeas Nothing)
                t4 = tabLink topic tab TabDelegation

              -- FIXME: we could see if we have any filter settings to save from another tab here.
              -- but if we did that, it would be nice to not lose the settings when moving back and
              -- forth between delegation and idea tabs, either.

            case phase of
                PhaseRefinement _ -> t1
                PhaseJury         -> t1
                PhaseVoting     _ -> t1 >> t2
                PhaseResult       -> t1 >> t2 >> t3 >> t4
  where
    phase   = topic ^. topicPhase
    topicId = topic ^. _Id
    space   = topic ^. topicIdeaSpace

instance FormPage CreateTopic where
    type FormPagePayload CreateTopic = ProtoTopic
    type FormPageResult CreateTopic = Topic

    formAction (CreateTopic space _ _) = U.Space space U.CreateTopic

    redirectOf (CreateTopic _ _ _) = U.listTopicIdeas

    makeForm (CreateTopic space ideas timestamp) =
        ProtoTopic
        <$> ("title" .: DF.text nil)
        <*> ("desc"  .: (Markdown <$> DF.text Nothing))
        <*> ("image" .: DF.text nil)
        <*> pure space
        <*> makeFormIdeaSelection ideas
        <*> pure timestamp

    formPage v form p@(CreateTopic _space ideas _timestamp) =
        semanticDiv p $ do
            div_ [class_ "container-main popup-page"] $ do
                div_ [class_ "container-narrow"] $ do
                    h1_ [class_ "main-heading"] "Thema erstellen"
                    form $ createOrEditTopic v ideas

createOrEditTopic :: Monad m => View (HtmlT m ()) -> [Idea] -> HtmlT m ()
createOrEditTopic v ideas = do
    label_ $ do
        span_ [class_ "label-text"] "Wie soll der Titel des Themas lauten?"
        inputText_ [class_ "m-small", placeholder_ "z.B. Computerraum"]
            "title" v
    label_ $ do
        span_ [class_ "label-text"] "Beschreiben Sie das Thema"
        inputTextArea_ [placeholder_ "Was haben die Ideen dieses Themas gemeinsam?"]
            Nothing Nothing "desc" v
    label_ $ do
        span_ [class_ "label-text"] "Fügen Sie weitere wilde dem neuen Thema hinzu"
        formPageIdeaSelection v ideas
        -- FIXME: mark the one with the quorum that triggered creating this
        -- topic as selected by default.  (see also: FIXME at makeFormIdeaSelection.)
    footer_ [class_ "form-footer"] $ do
        DF.inputSubmit "Veröffentlichen"


instance FormPage EditTopic where
    -- While the input page contains all the wild ideas the result page only contains
    -- the ideas to be added to the topic.
    type FormPagePayload EditTopic = EditTopicData

    formAction (EditTopic space topic _) = U.Space space $ U.MoveIdeasToTopic (topic ^. _Id)

    redirectOf (EditTopic _ topic _) _ = U.listTopicIdeas topic

    makeForm (EditTopic _space topic ideas) =
        EditTopicData
        <$> ("title" .: DF.text (Just (topic ^. topicTitle)))
        <*> ("desc"  .: ((topic ^. topicDesc) & _Markdown %%~ (DF.text . Just)))
        <*> makeFormIdeaSelection ideas

    formPage v form p@(EditTopic _space _topic ideas) = do
        semanticDiv p $ do
            div_ [class_ "container-main popup-page"] $ do
                div_ [class_ "container-narrow"] $ do
                    h1_ [class_ "main-heading"] "Thema bearbeiten"
                    form $ createOrEditTopic v ideas

ideaToFormField :: Idea -> ST
ideaToFormField idea = "idea-" <> cs (show $ idea ^. _Id)

-- FIXME: formPageIdeaSelection and makeFormIdeaSelection should be defined as a subform.
formPageIdeaSelection :: (Monad m) => View (HtmlT m ()) -> [Idea] -> HtmlT m ()
formPageIdeaSelection v ideas =
    ul_ . for_ ideas $ \idea ->
        li_ $ do
            DF.inputCheckbox (ideaToFormField idea) v
            idea ^. ideaTitle . html

-- FIXME: this is called both from CreateTopic and EditTopic.  the ideas listed here should include
-- wild ones in the surrounding space, plus those already in the topic.  the ones already in the
-- topic should be pre-selected.
makeFormIdeaSelection :: forall m v . (Monad m, Monoid v)
                      => [Idea] -> DF.Form v m [AUID Idea]
makeFormIdeaSelection ideas =
    fmap catMaybes . sequenceA $
        [ justIf (idea ^. _Id) <$> (ideaToFormField idea .: DF.bool Nothing)
        | idea <- ideas ]


-- * handlers

viewTopic :: (ActionPersist m, ActionUserHandler m, MonadError ActionExcept m)
    => ViewTopicTab -> AUID Topic -> m (Frame ViewTopic)
viewTopic tab = makeFrame . viewTopicPage tab

viewTopicPage :: (ActionPersist m, ActionUserHandler m, MonadError ActionExcept m)
    => ViewTopicTab -> AUID Topic -> m ViewTopic
viewTopicPage tab topicId = do
    ctx <- renderContext
    equery (do
        topic <- maybe404 =<< findTopic topicId
        case tab of
            TabDelegation ->
              do
                delegations <- findDelegationsByContext $ DlgCtxTopicId topicId
                pure $ ViewTopicDelegations topic delegations
            _ ->
              do
                ideas <- ideasFilterQuery (tab ^? viewTopicTabFilter . _Just)
                      <$> findIdeasByTopic topic
                ideasAndNumVoters <- ListItemIdeas
                    ctx
                    (tab ^? viewTopicTabFilter . _Just)
                    <$> (getListInfoForIdea `mapM` ideas)

                pure $ ViewTopicIdeas ctx tab topic ideasAndNumVoters)

-- FIXME: ProtoTopic also holds an IdeaSpace, which can introduce inconsistency.
createTopic :: ActionM m => IdeaSpace -> FormPageHandler m CreateTopic
createTopic space =
    FormPageHandler
        (do
            now <- getCurrentTimestamp
            query $ CreateTopic space
                <$> findWildIdeasBySpace space
                <*> phaseEndRefinement now)
        Action.createTopic

editTopic :: ActionM m => AUID Topic -> FormPageHandler m EditTopic
editTopic topicId = FormPageHandler getPage (update . Persistent.EditTopic topicId)
  where
    getPage = equery $ do
        topic <- maybe404 =<< findTopic topicId
        let space = topic ^. topicIdeaSpace
        ideas <- findWildIdeasBySpace space
        pure $ EditTopic space topic ideas

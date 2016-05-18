{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Page.Topic
    ( ViewTopic(..)
    , ViewTopicTab(..)
    , CreateTopic(..)
    , EditTopic(..)
    , IdeasFilterApi
    , viewTopic
    , createTopic
    , editTopic
    )
where

import Control.Category ((.))
import Control.Exception (assert)
import Data.List (sortBy)
import Prelude hiding ((.))

import Action (ActionM, ActionPersist(..), ActionUserHandler, ActionCurrentTimestamp, getCurrentTimestamp)
import Frontend.Fragment.IdeaList as IdeaList
import Frontend.Prelude
import Frontend.Validation hiding (space, tab)
import LifeCycle (TopicCapability(..), topicCapabilities)
import Persistent
    ( findDelegationsByContext
    , findIdeasByTopic
    , findIdeasByTopicId
    , findTopic
    , findTopic
    , findWildIdeasBySpace
    , getListInfoForIdea
    , maybe404
    , phaseEndRefinement
    , wildIdeasReachedQuorumBySpace
    )

import qualified Action (createTopic, editTopic)
import qualified Frontend.Constant as Constant
import qualified Frontend.Path as U
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Lucid.Html5 as DF


-- * types

data ViewTopicTab
  = TabIdeas { _topicTab :: ListIdeasInTopicTab, _viewTopicTabQuery :: IdeasQuery }
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
  = ViewTopicIdeas Timestamp RenderContext ViewTopicTab Topic ListItemIdeas
  | ViewTopicDelegations Timestamp RenderContext Topic [Delegation]
  deriving (Eq, Show, Read)

instance Page ViewTopic where
    extraBodyClasses _ = ["m-shadow"]

-- | 10.1 Create topic: Create topic
data CreateTopic = CreateTopic
    { _createTopicIdeaSpace   :: IdeaSpace
    , _createTopicIdeas       :: [Idea]
    , _createTopicRefPhaseEnd :: Timestamp }
  deriving (Eq, Show, Read)

instance Page CreateTopic

-- | 10.2 Create topic: Move ideas to topic (Edit topic)
-- FIXME: Edit topic page is used for editing a topic and move ideas to the topic.
data EditTopic = EditTopic IdeaSpace Topic [Idea] [AUID Idea]
  deriving (Eq, Show, Read)

instance Page EditTopic


-- * templates

tabLink :: Monad m => Topic -> ViewTopicTab -> ViewTopicTab -> HtmlT m ()
tabLink topic curTab targetTab =
  case targetTab of
    TabIdeas ListIdeasInTopicTabAll     _ -> ideaLnk  "tab-ideas"       "Alle Ideen"
    TabIdeas ListIdeasInTopicTabVoting  _ -> ideaLnk  "tab-voting"      "Ideen in der Abstimmung"
    TabIdeas ListIdeasInTopicTabWinning _ -> ideaLnk  "tab-winning"     "Gewinner"
    TabDelegation                         -> delegLnk "tab-delegations" "Beauftrage Stimmen"
  where
    ideaLnk  = lnk (U.listIdeasInTopic topic (targetTab ^?! topicTab) Nothing)
    delegLnk = lnk (U.Space (topic ^. topicIdeaSpace) . U.ViewTopicDelegations $ (topic ^. _Id))

    lnk url ident =
        a_ [ id_ ident
           , href_ url
           , class_ $ tabSelected (curTab ^? topicTab) (targetTab ^? topicTab)
           ]

instance ToHtml ViewTopic where
    toHtmlRaw = toHtml

    toHtml p@(ViewTopicDelegations now ctx topic delegations) = semanticDiv p $ do
        viewTopicHeaderDiv now ctx topic TabDelegation
        -- related: Frontend.Page.User.renderDelegations
        -- FIXME: implement!
        pre_ $ topic ^. showed . html
        pre_ $ delegations ^. showed . html

    toHtml p@(ViewTopicIdeas now ctx tab topic ideasAndNumVoters) = semanticDiv p $ do
        assert (tab /= TabDelegation) $ viewTopicHeaderDiv now ctx topic tab
        div_ [class_ "ideas-list"] $ toHtml ideasAndNumVoters


viewTopicHeaderDiv :: Monad m => Timestamp -> RenderContext -> Topic -> ViewTopicTab -> HtmlT m ()
viewTopicHeaderDiv now ctx topic tab = do
    let caps    = topicCapabilities phase (ctx ^. renderContextUser . userRole)
        phase   = topic ^. topicPhase
        topicId = topic ^. _Id
        space   = topic ^. topicIdeaSpace

    div_ [class_ $ "topic-header phase-" <> cs (show phase)] $ do
        header_ [class_ "detail-header"] $ do
            a_ [class_ "btn m-back detail-header-back", href_ $ U.Space space U.ListTopics] "Zu Allen Themen"
            let canEditTopic          = CanEditTopic          `elem` caps
                canPhaseForwardTopic  = CanPhaseForwardTopic  `elem` caps
                canPhaseBackwardTopic = CanPhaseBackwardTopic `elem` caps

            when (canEditTopic || canPhaseForwardTopic || canPhaseBackwardTopic) .
                nav_ [class_ "pop-menu m-dots detail-header-menu"] $ do
                    ul_ [class_ "pop-menu-list"] $ do
                        -- FIXME: There is no EditTopic path defined.
                        when canEditTopic .
                            li_ [class_ "pop-menu-list-item"] $ do
                                a_ [id_ "edit-topic",  href_ . U.Space space $ U.EditTopic topicId] $ do
                                    i_ [class_ "icon-pencil"] nil
                                    "Thema bearbeiten"
                        when canPhaseForwardTopic .
                            li_ [class_ "pop-menu-list-item m-form"] .
                                div_ [class_ "pop-menu-list-item-form-wrapper"] $ do
                                    i_ [class_ "icon-step-forward"] nil
                                    postLink_
                                        [class_ "btn-plain", onclickJs jsReloadOnClick]
                                        (U.Admin $ U.AdminTopicNextPhase topicId)
                                        "Nächste Phase"
                        when canPhaseBackwardTopic .
                            li_ [class_ "pop-menu-list-item m-form"] .
                                div_ [class_ "pop-menu-list-item-form-wrapper"] $ do
                                    i_ [class_ "icon-step-backward"] nil
                                    postLink_
                                        [class_ "btn-plain", onclickJs jsReloadOnClick]
                                        (U.Admin $ U.AdminTopicVotingPrevPhase topicId)
                                        "Vorherige Phase"

        h1_ [class_ "main-heading"] $ do
            span_ [class_ "sub-heading"] $ do
                phase ^. uilabeledST . html
                " "
                phase ^. displayPhaseTime now . html
            topic ^. topicTitle . html
        p_ [class_ "sub-header"] $ topic ^. topicDesc . html
        div_ [class_ "heroic-btn-group"] $ do
            let createIdeaButton = when (CanCreateIdea `elem` caps) .
                    a_ [ class_ "btn-cta heroic-cta"
                       , href_ . U.createIdea $ IdeaLocationTopic space topicId
                       ]
                        "+ Neue Idee"
                delegateVoteButton = when (CanVoteTopic `elem` caps) .
                    a_  [ class_ "btn-cta heroic-cta"
                        , href_ . U.Space space $ U.CreateTopicDelegation topicId
                        ] $ do
                      i_ [class_ "icon-bullhorn"] nil
                      "Stimme beauftragen"

            case phase of
                PhaseWildIdea{}   -> createIdeaButton
                PhaseRefinement{} -> createIdeaButton >> delegateVoteButton
                PhaseJury         -> delegateVoteButton
                PhaseVoting{}     -> delegateVoteButton
                PhaseResult       -> nil

        div_ [class_ "heroic-tabs"] $ do
            let t1 = tabLink topic tab (TabIdeas ListIdeasInTopicTabAll     emptyIdeasQuery)
                t2 = tabLink topic tab (TabIdeas ListIdeasInTopicTabVoting  emptyIdeasQuery)
                t3 = tabLink topic tab (TabIdeas ListIdeasInTopicTabWinning emptyIdeasQuery)
                t4 = tabLink topic tab TabDelegation

              -- FIXME: we could see if we have any filter settings to save from another tab here.
              -- but if we did that, it would be nice to not lose the settings when moving back and
              -- forth between delegation and idea tabs, either.

            case phase of
                PhaseWildIdea{}   -> t1
                PhaseRefinement{} -> t1
                PhaseJury         -> t1
                PhaseVoting{}     -> t1 >> t2
                PhaseResult       -> t1 >> t3 >> t4

displayPhaseTime :: Monoid r => Timestamp -> Getting r Phase String
displayPhaseTime now = phaseStatus . phaseLeftoverFrom now . to displayTimespan
  where
    displayTimespan t = case timespanDays t of
        -- n | n < 0 -> assert False $ error "displayPhaseTime"  (this breaks the test suite)
        0 -> "(Endet heute)"
        1 -> "(Endet morgen)"
        n -> "(Endet in " <> show n <> " Tagen)"

validateTopicTitle :: FormCS m r s
validateTopicTitle = validate "Title des Themas" title

validateTopicDesc :: forall m . Monad m => DF.Form (Html ()) m ST -> DF.Form (Html ()) m PlainDocument
validateTopicDesc =
    validate
        "Thema"
        (PlainDocument <$> (maxLength Constant.topicDescMaxLength . nonEmpty))

instance FormPage CreateTopic where
    type FormPagePayload CreateTopic = ProtoTopic
    type FormPageResult CreateTopic = Topic

    formAction (CreateTopic space _ _) = U.Space space U.CreateTopic

    redirectOf (CreateTopic _ _ _) topic = U.listIdeasInTopic topic ListIdeasInTopicTabAll Nothing

    makeForm CreateTopic{ _createTopicIdeaSpace
                        , _createTopicIdeas
                        , _createTopicRefPhaseEnd } =
        ProtoTopic
        <$> ("title" .: validateTopicTitle (DF.text nil))
        <*> ("desc"  .: validateTopicDesc  (DF.text nil))
        <*> ("image" .: DF.text nil) -- FIXME: validation
        <*> pure _createTopicIdeaSpace
        <*> makeFormIdeaSelection [] _createTopicIdeas
        <*> pure _createTopicRefPhaseEnd

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
        span_ [class_ "label-text"] $ if null ideas
            then "Noch keine wilden Ideen haben das Quorum erreicht"
            else "Fügen Sie weitere wilde dem neuen Thema hinzu"
        formPageIdeaSelection v ideas
        -- FIXME: mark the one with the quorum that triggered creating this
        -- topic as selected by default.  (see also: FIXME at makeFormIdeaSelection.)
    footer_ [class_ "form-footer"] $ do
        DF.inputSubmit "Veröffentlichen"

instance FormPage EditTopic where
    -- While the input page contains all the wild ideas the result page only contains
    -- the ideas to be added to the topic.
    type FormPagePayload EditTopic = EditTopicData

    formAction (EditTopic space topic _ _) = U.Space space $ U.EditTopic (topic ^. _Id)

    redirectOf (EditTopic _ topic _ _) _ = U.listIdeasInTopic topic ListIdeasInTopicTabAll Nothing

    makeForm (EditTopic _space topic ideas preselected) =
        EditTopicData
        <$> ("title" .: validateTopicTitle (DF.text . Just $ topic ^. topicTitle))
        <*> ("desc"  .: validateTopicDesc  (DF.text (topic ^. topicDesc . to unDescription . to Just)))
        <*> makeFormIdeaSelection preselected ideas

    formPage v form p@(EditTopic _space _topic ideas _preselected) = do
        semanticDiv p $ do
            div_ [class_ "container-main popup-page"] $ do
                div_ [class_ "container-narrow"] $ do
                    h1_ [class_ "main-heading"] "Thema bearbeiten"
                    form $ createOrEditTopic v ideas

ideaToFormField :: Idea -> ST
ideaToFormField idea = "idea-" <> idea ^. _Id . showed . csi

-- | FIXME: formPageIdeaSelection and makeFormIdeaSelection should be defined as a subform.
--
-- This form is called both from CreateTopic and EditTopic.  The ideas listed here include all wild
-- ones in the surrounding space, plus those already in the topic.  The ones already in the topic
-- are pre-selected.
formPageIdeaSelection :: (Monad m) => View (HtmlT m ()) -> [Idea] -> HtmlT m ()
formPageIdeaSelection v ideas =
    ul_ . for_ (sortBy (compare `on` view ideaTitle) ideas) $ \idea ->
        li_ $ do
            DF.inputCheckbox (ideaToFormField idea) v
            idea ^. ideaTitle . html

makeFormIdeaSelection :: forall m v . (Monad m, Monoid v)
                      => [AUID Idea] -> [Idea] -> DF.Form v m [AUID Idea]
makeFormIdeaSelection preselected ideas =
    fmap catMaybes . sequenceA $
        [ let checked = Just $ idea ^. _Id `elem` preselected
          in justIf (idea ^. _Id) <$> (ideaToFormField idea .: DF.bool checked)
        | idea <- ideas ]


-- * handlers

ideaFilterForTab :: ViewTopicTab -> [Idea] -> [Idea]
ideaFilterForTab = \case
    TabIdeas ListIdeasInTopicTabWinning _ -> filter isWinning
    TabIdeas ListIdeasInTopicTabVoting  _ -> filter isFeasibleIdea
    _                                     -> id

viewTopic :: (ActionPersist m, ActionUserHandler m, ActionCurrentTimestamp m)
    => ViewTopicTab -> AUID Topic -> m ViewTopic
viewTopic tab topicId = do
    ctx <- renderContext
    now <- getCurrentTimestamp
    equery (do
        topic <- maybe404 =<< findTopic topicId
        case tab of
            TabDelegation ->
                ViewTopicDelegations now ctx topic
                    <$> findDelegationsByContext (DlgCtxTopicId topicId)
            _ ->
              do
                let loc = topicIdeaLocation topic
                    ideasQuery = fromMaybe (assert False $ error "viewTopic: impossible.")
                               $ tab ^? viewTopicTabQuery
                ideas <- applyFilter ideasQuery . ideaFilterForTab tab
                         <$> findIdeasByTopic topic
                let topicTabKind = fromMaybe (error "viewTopic: impossible (2).")
                                 $ tab ^? topicTab
                ideasAndNumVoters <-
                    ListItemIdeas ctx (IdeaInViewTopic topicTabKind) loc ideasQuery
                    <$> getListInfoForIdea `mapM` ideas

                pure $ ViewTopicIdeas now ctx tab topic ideasAndNumVoters)

-- FIXME: ProtoTopic also holds an IdeaSpace, which can introduce inconsistency.
createTopic :: ActionM m => IdeaSpace -> FormPageHandler m CreateTopic
createTopic space =
    formPageHandlerCalcMsg
        (do
            now <- getCurrentTimestamp
            query $ CreateTopic space
                <$> wildIdeasReachedQuorumBySpace space
                <*> phaseEndRefinement now)
        Action.createTopic
        (\_ _ topic -> unwords ["Das Thema", topic ^. topicTitle . showed, "wurde angelegt."])

editTopic :: ActionM m => AUID Topic -> FormPageHandler m EditTopic
editTopic topicId =
    formPageHandlerWithMsg
        getPage
        (Action.editTopic topicId)
        "Das Thema wurde gespeichert."
  where
    getPage = equery $ do
        topic <- maybe404 =<< findTopic topicId
        let space = topic ^. topicIdeaSpace
        wildIdeas <- findWildIdeasBySpace space
        ideasInTopic <- findIdeasByTopicId topicId
        pure $ EditTopic space topic (wildIdeas <> ideasInTopic) (view _Id <$> ideasInTopic)

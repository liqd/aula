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
import Data.List (sortBy)
import Prelude hiding ((.))

import Action (ActionM, ActionPersist(..), ActionUserHandler, getCurrentTimestamp)
import Control.Exception (assert)
import Frontend.Fragment.IdeaList
import Frontend.Prelude hiding (moveIdeasToLocation, editTopic)
import Frontend.Validation hiding (space, tab)
import LifeCycle (TopicCapability(..), topicCapabilities)

import qualified Action (createTopic)
import qualified Frontend.Constant as Constant
import qualified Frontend.Path as U
import qualified Persistent.Api as Persistent (EditTopic(EditTopic))
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Lucid.Html5 as DF


-- * types

data ViewTopicTab
  = TabAllIdeas     { _viewTopicTabQuery :: IdeasQuery }
  | TabVotingIdeas  { _viewTopicTabQuery :: IdeasQuery }
  | TabWinningIdeas { _viewTopicTabQuery :: IdeasQuery }
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
  | ViewTopicDelegations RenderContext Topic [Delegation]
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

    toHtml p@(ViewTopicDelegations ctx topic delegations) = semanticDiv p $ do
        viewTopicHeaderDiv ctx topic TabDelegation
        -- related: Frontend.Page.User.renderDelegations
        -- FIXME: implement!
        pre_ $ topic ^. showed . html
        pre_ $ delegations ^. showed . html

    toHtml p@(ViewTopicIdeas ctx tab topic ideasAndNumVoters) = semanticDiv p $ do
        assert (tab /= TabDelegation) $ viewTopicHeaderDiv ctx topic tab
        div_ [class_ "ideas-list"] $ toHtml ideasAndNumVoters


viewTopicHeaderDiv :: Monad m => RenderContext -> Topic -> ViewTopicTab -> HtmlT m ()
viewTopicHeaderDiv ctx topic tab = do
    let caps = topicCapabilities phase (ctx ^. renderContextUser . userRole)
    div_ [class_ $ "topic-header phase-" <> cs (show (topic ^. topicPhase))] $ do
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
                                a_ [id_ "edit-topic",  href_ . U.Space space $ U.MoveIdeasToTopic topicId] $ do
                                    i_ [class_ "icon-pencil"] nil
                                    "Thema bearbeiten" -- <- Edit
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

        h1_   [class_ "main-heading"] $ do
            span_ [class_ "sub-heading"] $ phase ^. uilabeledST . html
            topic ^. topicTitle . html
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
                PhaseWildIdea     -> createIdeaButton
                PhaseWildFrozen   -> createIdeaButton
                PhaseRefinement{} -> createIdeaButton >> delegateVoteButton
                PhaseRefFrozen{}  -> createIdeaButton >> delegateVoteButton
                PhaseJury         -> delegateVoteButton
                PhaseVoting{}     -> delegateVoteButton
                PhaseVotFrozen{}  -> delegateVoteButton
                PhaseResult       -> nil

        div_ [class_ "heroic-tabs"] $ do
            let t1 = tabLink topic tab (TabAllIdeas emptyIdeasQuery)
                t2 = tabLink topic tab (TabVotingIdeas emptyIdeasQuery)
                t3 = tabLink topic tab (TabWinningIdeas emptyIdeasQuery)
                t4 = tabLink topic tab TabDelegation

              -- FIXME: we could see if we have any filter settings to save from another tab here.
              -- but if we did that, it would be nice to not lose the settings when moving back and
              -- forth between delegation and idea tabs, either.

            case phase of
                PhaseWildIdea     -> t1
                PhaseWildFrozen   -> t1
                PhaseRefinement{} -> t1
                PhaseRefFrozen{}  -> t1
                PhaseJury         -> t1
                PhaseVoting{}     -> t1 >> t2
                PhaseVotFrozen{}  -> t1 >> t2
                PhaseResult       -> t1 >> t2 >> t3 >> t4
  where
    phase   = topic ^. topicPhase
    topicId = topic ^. _Id
    space   = topic ^. topicIdeaSpace

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

    redirectOf (CreateTopic _ _ _) = U.listTopicIdeas

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
            then "No wild ideas have reached the quorum yet" -- TODO: Translate
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

    formAction (EditTopic space topic _ _) = U.Space space $ U.MoveIdeasToTopic (topic ^. _Id)

    redirectOf (EditTopic _ topic _ _) _ = U.listTopicIdeas topic

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

viewTopic :: (ActionPersist m, ActionUserHandler m)
    => ViewTopicTab -> AUID Topic -> m ViewTopic
viewTopic tab topicId = do
    ctx <- renderContext
    equery (do
        topic <- maybe404 =<< findTopic topicId
        case tab of
            TabDelegation ->
                ViewTopicDelegations ctx topic
                    <$> findDelegationsByContext (DlgCtxTopicId topicId)
            _ ->
              do
                let loc = topicIdeaLocation topic
                    ideasQuery = fromMaybe (assert False $ error "viewTopic: impossible.")
                               $ tab ^? viewTopicTabQuery
                ideas <- applyFilter ideasQuery <$> findIdeasByTopic topic
                ideasAndNumVoters <- ListItemIdeas ctx IdeaInViewTopic loc ideasQuery <$>
                                            (getListInfoForIdea `mapM` ideas)

                pure $ ViewTopicIdeas ctx tab topic ideasAndNumVoters)

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
        (update . Persistent.EditTopic topicId)
        "Das Thema wurde gespeichert."
  where
    getPage = equery $ do
        topic <- maybe404 =<< findTopic topicId
        let space = topic ^. topicIdeaSpace
        wildIdeas <- wildIdeasReachedQuorumBySpace space
        ideasInTopic <- findIdeasByTopicId topicId
        pure $ EditTopic space topic (wildIdeas <> ideasInTopic) (view _Id <$> ideasInTopic)

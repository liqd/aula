{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Page.Topic
    ( ViewTopic(..), _ViewTopicIdeas, _ViewTopicDelegations
    , vtNow, vtCtx, vtTab, vtTopic, vtIdeas, vtDelegations
    , ViewTopicTab(..)
    , CreateTopic(..), _CreateTopic, ctCtx, ctIdeaSpace, ctIdeas, ctRefPhaseEnd
    , EditTopic(..), _EditTopic, etCtx, etIdeaSpace, etTopic, etIdeasStats, etIdeas
    , IdeasFilterApi
    , viewTopic
    , createTopic
    , editTopic
    )
where

import Control.Category ((.))
import Control.Exception (assert)
import Data.List (sortBy)
import Data.Time
import Prelude hiding ((.))

import Action (ActionM, ActionPersist(..), ActionUserHandler, ActionCurrentTimestamp,
               spaceCapCtx, topicCapCtx, getCurrentTimestamp)
import Config (unsafeTimestampToLocalTime, aulaTimeLocale)
import Frontend.Fragment.IdeaList as IdeaList
import Frontend.Prelude
import Frontend.Validation hiding (space, tab)
import LifeCycle (Capability(..), CapCtx(..), capabilities)
import Persistent
    ( findDelegationsByContext
    , findIdeasByTopic
    , findIdeasByTopicId
    , findWildIdeasBySpace
    , IdeaStats(..)
    , ideaReachedQuorum
    , ideaStatsIdea
    , getIdeaStats
    , phaseEndRefinement
    , ideaAccepted
    )

import qualified Action (createTopic, editTopic)
import qualified Frontend.Constant as Constant
import qualified Frontend.Path as U
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Lucid.Html5 as DF


-- * types

data ViewTopicTab
  = TabIdeas { _topicTab :: ListIdeasInTopicTab, viewTopicTabQuery :: IdeasQuery }
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
  = ViewTopicIdeas
    { _vtNow         :: Timestamp
    , _vtCtx         :: CapCtx
    , _vtTab         :: ViewTopicTab
    , _vtTopic       :: Topic
    , _vtIdeas       :: ListItemIdeas
    }
  | ViewTopicDelegations
    { _vtNow         :: Timestamp
    , _vtCtx         :: CapCtx
    , _vtTopic       :: Topic
    , _vtDelegations :: [Delegation]
    }
  deriving (Eq, Show, Read)

makeLenses ''ViewTopic
makePrisms ''ViewTopic

instance Page ViewTopic where
    isAuthorized = authNeedCaps [CanViewTopic] vtCtx
    extraBodyClasses _ = ["m-shadow"]

-- | 10.1 Create topic: Create topic
data CreateTopic = CreateTopic
    { _ctCtx         :: CapCtx
    , _ctIdeaSpace   :: IdeaSpace
    , _ctIdeas       :: [IdeaStats]
    , _ctRefPhaseEnd :: Timestamp }
  deriving (Eq, Show, Read)

makeLenses ''CreateTopic
makePrisms ''CreateTopic

instance Page CreateTopic where
    isAuthorized = authNeedCaps [CanCreateTopic] ctCtx

-- | 10.2 Create topic: Move ideas to topic (Edit topic)
-- FIXME: Edit topic page is used for editing a topic and move ideas to the topic.
data EditTopic = EditTopic
    { _etCtx        :: CapCtx
    , _etIdeaSpace  :: IdeaSpace
    , _etTopic      :: Topic
    , _etIdeasStats :: [IdeaStats]
    , _etIdeas      :: [AUID Idea]
    }
  deriving (Eq, Show, Read)

makeLenses ''EditTopic
makePrisms ''EditTopic

instance Page EditTopic where
    isAuthorized = authNeedCaps [CanEditTopic] etCtx


-- * templates

tabLink :: Monad m => Topic -> ViewTopicTab -> ViewTopicTab -> HtmlT m ()
tabLink topic curTab targetTab =
  case targetTab of
    TabIdeas ListIdeasInTopicTabAll      _ -> ideaLnk  "tab-ideas"       "Alle Ideen"
    TabIdeas ListIdeasInTopicTabVoting   _ -> ideaLnk  "tab-voting"      "Ideen in der Abstimmung"
    TabIdeas ListIdeasInTopicTabAccepted _ -> ideaLnk  "tab-voting"      "Angenommene Ideen"
    TabIdeas ListIdeasInTopicTabWinning  _ -> ideaLnk  "tab-winning"     "Gewinner"
    TabDelegation                          -> delegLnk "tab-delegations" "Beauftrage Stimmen"
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


viewTopicHeaderDiv :: Monad m => Timestamp -> CapCtx -> Topic -> ViewTopicTab -> HtmlT m ()
viewTopicHeaderDiv now ctx topic tab = do
    let caps    = capabilities ctx
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
                                        [class_ "btn-plain", jsReloadOnClick]
                                        (U.Admin $ U.AdminTopicNextPhase topicId)
                                        "Nächste Phase"
                        when canPhaseBackwardTopic .
                            li_ [class_ "pop-menu-list-item m-form"] .
                                div_ [class_ "pop-menu-list-item-form-wrapper"] $ do
                                    i_ [class_ "icon-step-backward"] nil
                                    postLink_
                                        [class_ "btn-plain", jsReloadOnClick]
                                        (U.Admin $ U.AdminTopicVotingPrevPhase topicId)
                                        "Vorherige Phase"

        h1_ [class_ "main-heading"] $ do
            span_ [class_ "sub-heading"] $ do
                phase ^. uilabeledST . html
                " "
                phase ^. displayPhaseTime now . html
            topic ^. topicTitle . html
        div_ [class_ "sub-header"] $ topic ^. topicDesc . html
        div_ [class_ "heroic-btn-group"] $ do
            let createIdeaButton = when (CanCreateIdea `elem` caps) .
                    a_ [ class_ "btn-cta heroic-cta m-large"
                       , href_ . U.createIdea $ IdeaLocationTopic space topicId
                       ] $
                      "+ Neue Idee"
                delegateVoteButton = when (CanVote `elem` caps) .
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
            let t1 = tabLink topic tab (TabIdeas ListIdeasInTopicTabAll      emptyIdeasQuery)
                t2 = tabLink topic tab (TabIdeas ListIdeasInTopicTabVoting   emptyIdeasQuery)
                t3 = tabLink topic tab (TabIdeas ListIdeasInTopicTabAccepted emptyIdeasQuery)
                t4 = tabLink topic tab (TabIdeas ListIdeasInTopicTabWinning  emptyIdeasQuery)
                t5 = tabLink topic tab TabDelegation

              -- FIXME: we could see if we have any filter settings to save from another tab here.
              -- but if we did that, it would be nice to not lose the settings when moving back and
              -- forth between delegation and idea tabs, either.

            case phase of
                PhaseWildIdea{}   -> t1                   >> t5
                PhaseRefinement{} -> t1                   >> t5
                PhaseJury         -> t1                   >> t5
                PhaseVoting{}     -> t1 >> t2 >> t3       >> t5
                PhaseResult       -> t1       >> t3 >> t4 >> t5

displayPhaseTime :: Monoid r => Timestamp -> Getting r Phase String
displayPhaseTime now = phaseStatus . to info
  where
    info t@(ActivePhase stamp) =
        "(Endet " <> displayTimespan t <> showStamp stamp <> ")"
    info t@(FrozenPhase _) =
        "(Endet " <> displayTimespanFrozen t <> ")"

    displayTimespan st = case stampToDays st of
        -- n | n < 0 -> assert False $ error "displayPhaseTime"  -- (this breaks the test suite)
        0 -> "heute"
        1 -> "morgen"
        n -> "in " <> show n <> " Tagen"

    displayTimespanFrozen st = (cs . show . stampToDays $ st) <> " Tage nach den Ferien"
    stampToDays st = timespanDays (st ^. phaseLeftoverFrom now) + 1
    showStamp = formatTime aulaTimeLocale " am %F um ca. %H Uhr %Z" . unsafeTimestampToLocalTime

validateTopicTitle :: FormCS m r s
validateTopicTitle = validate "Title des Themas" titleV

validateTopicDesc :: forall m . Monad m => DF.Form (Html ()) m ST -> DF.Form (Html ()) m PlainDocument
validateTopicDesc =
    validate
        "Thema"
        (PlainDocument <$> (maxLengthV Constant.topicDescMaxLength . nonEmptyV))

instance FormPage CreateTopic where
    type FormPagePayload CreateTopic = ProtoTopic
    type FormPageResult CreateTopic = Topic

    formAction ct = U.Space (ct ^. ctIdeaSpace) U.CreateTopic
    redirectOf _ topic = U.listIdeasInTopic topic ListIdeasInTopicTabAll Nothing

    makeForm ct =
        ProtoTopic
        <$> ("title" .: validateTopicTitle (DF.text nil))
        <*> ("desc"  .: validateTopicDesc  (DF.text nil))
        <*> ("image" .: DF.text nil) -- FIXME: validation
        <*> pure (ct ^. ctIdeaSpace)
        <*> makeFormIdeaSelection [] (ct ^.. ctIdeas . each . ideaStatsIdea)
        <*> pure (ct ^. ctRefPhaseEnd)

    formPage v form ct =
        semanticDiv ct $ do
            div_ [class_ "container-main popup-page"] $ do
                div_ [class_ "container-narrow"] $ do
                    h1_ [class_ "main-heading"] "Thema erstellen"
                    form . createOrEditTopic v $ ct ^. ctIdeas

createOrEditTopic :: Monad m => View (HtmlT m ()) -> [IdeaStats] -> HtmlT m ()
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
            then "Es stehen keine Ideen zur Auswahl."
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

    formAction (EditTopic _ space topic _ _) = U.Space space $ U.EditTopic (topic ^. _Id)
    redirectOf et _ = U.listIdeasInTopic (et ^. etTopic) ListIdeasInTopicTabAll Nothing

    makeForm (EditTopic _ctx _space topic ideas preselected) =
        EditTopicData
        <$> ("title" .: validateTopicTitle (DF.text . Just $ topic ^. topicTitle))
        <*> ("desc"  .: validateTopicDesc  (DF.text (topic ^. topicDesc . to unDescription . to Just)))
        <*> makeFormIdeaSelection preselected (_ideaStatsIdea <$> ideas)

    formPage v form et = do
        semanticDiv et $ do
            div_ [class_ "container-main popup-page"] $ do
                div_ [class_ "container-narrow"] $ do
                    h1_ [class_ "main-heading"] "Thema bearbeiten"
                    form . createOrEditTopic v $ et ^. etIdeasStats

ideaToFormField :: Idea -> ST
ideaToFormField idea = "idea-" <> idea ^. _Id . showed . csi

-- | FIXME: formPageIdeaSelection and makeFormIdeaSelection should be defined as a subform.
--
-- This form is called both from CreateTopic and EditTopic.  The ideas listed here include all wild
-- ones in the surrounding space, plus those already in the topic.  The ones already in the topic
-- are pre-selected.
formPageIdeaSelection :: (Monad m) => View (HtmlT m ()) -> [IdeaStats] -> HtmlT m ()
formPageIdeaSelection v ideaStats =
    table_ [class_ "admin-table", style_ "padding: 30px"] .
      for_ (sortBy (compare `on` view (ideaStatsIdea . ideaTitle)) ideaStats) $ \ideaStat ->
          tr_ $ do
              td_ $ do
                  DF.inputCheckbox
                      (ideaToFormField $ ideaStat ^. ideaStatsIdea)
                      v
              td_ $ do
                  ideaStat ^. ideaStatsIdea . ideaTitle . html
              td_ . when (ideaReachedQuorum ideaStat) $ do
                  img_ [src_ . U.TopStatic $ "images/badge_aufdemtisch.png", width_ "31"]


makeFormIdeaSelection :: forall m v . (Monad m, Monoid v)
                      => [AUID Idea] -> [Idea] -> DF.Form v m [AUID Idea]
makeFormIdeaSelection preselected ideas =
    fmap catMaybes . sequenceA $
        [ let checked = Just $ idea ^. _Id `elem` preselected
          in justIf (idea ^. _Id) <$> (ideaToFormField idea .: DF.bool checked)
        | idea <- ideas ]


-- * handlers

ideaFilterForTab :: ListIdeasInTopicTab -> [IdeaStats] -> [IdeaStats]
ideaFilterForTab = \case
    ListIdeasInTopicTabAll      -> id
    ListIdeasInTopicTabVoting   -> filter fea . filter (not . acc)
    ListIdeasInTopicTabAccepted -> filter acc
    ListIdeasInTopicTabWinning  -> filter win
  where
    win = isWinning      . view ideaStatsIdea
    fea = isFeasibleIdea . view ideaStatsIdea
    acc = ideaAccepted

viewTopic :: (ActionPersist m, ActionUserHandler m, ActionCurrentTimestamp m)
    => ViewTopicTab -> AUID Topic -> m ViewTopic
viewTopic tab topicId = do
    now <- getCurrentTimestamp
    (ctx, topic) <- topicCapCtx topicId
    equery $
        case tab of
            TabDelegation ->
                ViewTopicDelegations now ctx topic
                    <$> findDelegationsByContext (DlgCtxTopicId topicId)
            TabIdeas ideasTab ideasQuery -> do
                let loc = topicIdeaLocation topic
                ideas <- applyFilter ideasQuery . ideaFilterForTab ideasTab
                     <$> (findIdeasByTopic topic >>= mapM getIdeaStats)

                let listItemIdeas =
                        ListItemIdeas ctx (IdeaInViewTopic ideasTab) loc ideasQuery ideas

                pure $ ViewTopicIdeas now ctx tab topic listItemIdeas

-- FIXME: ProtoTopic also holds an IdeaSpace, which can introduce inconsistency.
createTopic :: ActionM m => IdeaSpace -> FormPageHandler m CreateTopic
createTopic space =
    formPageHandlerCalcMsg
        (do ctx <- spaceCapCtx space
            now <- getCurrentTimestamp
            equery $ CreateTopic ctx space
                <$> (mapM getIdeaStats =<< findWildIdeasBySpace space)
                <*> phaseEndRefinement now)
        Action.createTopic
        (\_ _ topic -> unwords ["Das Thema", topic ^. topicTitle . showed, "wurde angelegt."])

editTopic :: ActionM m => AUID Topic -> FormPageHandler m EditTopic
editTopic topicId =
    formPageHandlerWithMsg
        (getPage =<< topicCapCtx topicId)
        (Action.editTopic topicId)
        "Das Thema wurde gespeichert."
  where
    getPage (ctx, topic) = equery $ do
        let space = topic ^. topicIdeaSpace
        wildIdeas <- mapM getIdeaStats =<< findWildIdeasBySpace space
        ideasInTopic <- mapM getIdeaStats =<< findIdeasByTopicId topicId
        pure $ EditTopic
                ctx
                space
                topic
                (wildIdeas <> ideasInTopic)
                (view (ideaStatsIdea . _Id) <$> ideasInTopic)

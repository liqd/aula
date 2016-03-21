{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Page.Topic
    ( ViewTopic(..)
    , ViewTopicTab(..)
    , CreateTopic(..)
    , MoveIdeasToTopic(..)
    , EditTopic(..)
    , viewTopic
    , createTopic
    , editTopic
    , moveIdeasToTopic )
where

import Action (ActionM, ActionPersist(..), ActionUserHandler, ActionExcept, currentUserAddDb)
import Control.Exception (assert)
import Frontend.Prelude hiding (moveIdeasToLocation)

import qualified Persistent
import qualified Frontend.Path as U
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Lucid.Html5 as DF


-- * types

data ViewTopicTab
  = TabAllIdeas
  | TabVotingIdeas
  | TabWinningIdeas
  | TabDelegation
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | 4 Topic overview
-- * 4.1 Topic overview: Refinement phase
-- * 4.2 Topic overview: Jury (assessment) phase
-- * 4.3 Topic overview: Voting phase
-- * 4.4 Topic overview: Result phase
-- * 4.5 Topic overview: Delegations
data ViewTopic
  = ViewTopicIdeas ViewTopicTab Topic [(Idea, Int)]
  | ViewTopicDelegations Topic [Delegation]
  deriving (Eq, Show, Read)

instance Page ViewTopic

-- | 10.1 Create topic: Create topic
data CreateTopic = CreateTopic IdeaSpace [AUID Idea]
  deriving (Eq, Show, Read)

instance Page CreateTopic

-- | 10.2 Create topic: Move ideas to topic (Edit topic)
data MoveIdeasToTopic = MoveIdeasToTopic IdeaSpace (AUID Topic) [Idea]
  deriving (Eq, Show, Read)

instance Page MoveIdeasToTopic

-- | 10.3 ???
-- FIXME: Which page is this in the click-dummy?
data EditTopic = EditTopic
  deriving (Eq, Show, Read)

instance Page EditTopic


-- * templates

tabLink :: Monad m => Topic -> ViewTopicTab -> ViewTopicTab -> HtmlT m ()
tabLink topic curTab targetTab =
  case targetTab of
    TabAllIdeas     -> go "tab-ideas"       U.ListTopicIdeas        "Alle Ideen"
    TabVotingIdeas  -> go "tab-voting"      U.ViewTopicIdeasVoting  "Ideen in der Abstimmung"
    TabWinningIdeas -> go "tab-winning"     U.ViewTopicIdeasWinning "Gewinner"
    TabDelegation   -> go "tab-delegations" U.ViewTopicDelegations  "Beauftragen Stimmen"
  where
    space = topic ^. topicIdeaSpace
    go ident uri =
        a_ [ id_ ident
           , href_ . U.Space space . uri $ (topic ^. _Id)
           , class_ $ tabSelected curTab targetTab
           ]

instance ToHtml EditTopic where
    toHtmlRaw = toHtml
    toHtml p@EditTopic = semanticDiv p "Edit Topic" -- FIXME

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

    toHtml p@(ViewTopicIdeas tab topic ideasAndNumVoters) = semanticDiv p $ do
        assert (tab /= TabDelegation) $ viewTopicHeaderDiv topic tab
        div_ [class_ "m-shadow"] $ do
            div_ [class_ "ideas-list"] $ do
                a_ [class_ "btn-settings", href_ U.Broken] $ do  -- not sure what settings are meant here?
                    i_ [class_ "icon-sort", title_ "Settings"] nil
                for_ ideasAndNumVoters $ \(idea, numVoters) ->
                    ListItemIdea True (Just (topic ^. topicPhase)) numVoters idea ^. html

viewTopicHeaderDiv :: Monad m => Topic -> ViewTopicTab -> HtmlT m ()
viewTopicHeaderDiv topic tab = do
    div_ [class_ "topic-header"] $ do
        header_ [class_ "detail-header"] $ do
            a_ [class_ "btn m-back detail-header-back", href_ $ U.Space space U.ListTopics] "Zu Allen Themen"
            nav_ [class_ "pop-menu m-dots detail-header-menu"] $ do
                ul_ [class_ "pop-menu-list"] $ do
                    li_ [class_ "pop-menu-list-item"] $ do
                        a_ [id_ "edit-topic",  href_ . U.Space space $ U.EditTopic topicId] $ do
                            i_ [class_ "icon-pencil"] nil
                            "Thema bearbeiten"
        h1_   [class_ "main-heading"] $ do
            span_ [class_ "sub-heading"] . toHtml $ phaseName phase
            toHtml $ topic ^. topicTitle
        p_ [class_ "sub-header"] $ topic ^. topicDesc . html
        when (phase == PhaseRefinement) $
            a_ [class_ "btn-cta heroic-cta", href_ . U.Space space $ U.CreateTopicIdea topicId] "+ Neue Idee"
        when (phase < PhaseResult) .
            a_  [class_ "btn-cta heroic-cta", href_ . U.Space space $ U.CreateTopicDelegation topicId] $ do
                i_ [class_ "icon-bullhorn"] nil
                "Stimme Beauftragen"
        div_ [class_ "heroic-tabs"] $ do
            tabLink topic tab TabAllIdeas
            when ((topic ^. topicPhase) >= PhaseVoting) $ tabLink topic tab TabVotingIdeas
            when ((topic ^. topicPhase) >= PhaseResult) $ tabLink topic tab TabWinningIdeas
            tabLink topic tab TabDelegation
  where
    phase   = topic ^. topicPhase
    topicId = topic ^. _Id
    space   = topic ^. topicIdeaSpace

instance FormPage CreateTopic where
    type FormPageResult CreateTopic = ProtoTopic

    formAction (CreateTopic space _) = relPath $ U.Space space U.CreateTopic
    redirectOf (CreateTopic space _) = relPath $ U.Space space U.ListTopics

    makeForm (CreateTopic space ideas) =
        ProtoTopic
        <$> ("title" .: DF.text nil)
        <*> ("desc"  .: (Markdown <$> DF.text Nothing))
        <*> ("image" .: DF.text nil)
        <*> pure space
        <*> pure ideas

    formPage v fa p =
        semanticDiv p $ do
            h3_ "Create Topic"
            DF.form v fa $ do
                DF.inputText     "title" v >> br_ []
                DF.inputTextArea Nothing Nothing "desc" v >> br_ []
                DF.inputText     "image" v >> br_ []
                DF.inputSubmit   "Add Topic"

instance FormPage MoveIdeasToTopic where
    -- While the input page contains all the wild ideas the result page only contains
    -- the ideas to be added to the topic.
    type FormPageResult MoveIdeasToTopic = [AUID Idea]

    formAction (MoveIdeasToTopic space topicId _) = relPath . U.Space space $ U.ListTopicIdeas topicId
    redirectOf (MoveIdeasToTopic space topicId _) = relPath . U.Space space $ U.ListTopicIdeas topicId

    makeForm (MoveIdeasToTopic _ _ ideas) =
        fmap catMaybes . sequenceA $
        [ justIf (idea ^. _Id) <$> (ideaToFormField idea .: DF.bool Nothing) | idea <- ideas ]

    formPage v fa p@(MoveIdeasToTopic _ _ ideas) = do
        semanticDiv p $ do
            h3_ "Wählen Sie weitere Ideen aus"
            DF.form v fa $ do
                ul_ $ do
                    for_ ideas $ \idea ->
                        li_ $ do
                            DF.inputCheckbox (ideaToFormField idea) v
                            idea ^. ideaTitle . html
                DF.inputSubmit "Speichern"
                button_ "Abbrechen" -- FIXME

ideaToFormField :: Idea -> ST
ideaToFormField idea = "idea-" <> cs (show $ idea ^. _Id)


-- * handlers

viewTopic :: (ActionPersist r m, ActionUserHandler m, MonadError ActionExcept m)
    => ViewTopicTab -> AUID Topic -> m (Frame ViewTopic)
viewTopic tab topicId = makeFrame =<< persistent (do
    Just topic <- findTopic topicId  -- FIXME: 404
    delegations <- findDelegationsByContext $ DelCtxTopic topicId
    case tab of
        TabDelegation -> pure $ ViewTopicDelegations topic delegations
        _ -> ViewTopicIdeas tab topic <$> (findIdeasByTopic topic >>= mapM getNumVotersForIdea))

createTopic :: ActionM r m => IdeaSpace -> [AUID Idea] -> ServerT (FormHandler CreateTopic) m
createTopic space ideas =
  redirectFormHandler (pure $ CreateTopic space ideas) (currentUserAddDb addTopic)

moveIdeasToTopic :: ActionM r m => AUID Topic -> ServerT (FormHandler MoveIdeasToTopic) m
moveIdeasToTopic topicId = redirectFormHandler getPage addIdeas
  where
    getPage = persistent $ do
        Just space <- view topicIdeaSpace <$$> findTopic topicId  -- FIXME: 404
        ideas <- findWildIdeasBySpace space
        pure $ MoveIdeasToTopic space topicId ideas
    addIdeas ideas = persistent $ do
        Just space <- view topicIdeaSpace <$$> findTopic topicId  -- FIXME: 404
        Persistent.moveIdeasToLocation ideas (IdeaLocationTopic space topicId)

editTopic :: ActionM r m => AUID Topic -> m (Frame EditTopic)
editTopic _ = makeFrame EditTopic

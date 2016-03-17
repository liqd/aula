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

import Action (ActionM, ActionPersist(..), ActionUserHandler, ActionExcept, currentUser)
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
  deriving (Eq, Show, Read, Enum, Bounded)

-- | 4 Topic overview
-- * 4.1 Topic overview: Refinement phase
-- * 4.2 Topic overview: Jury (assessment) phase
-- * 4.3 Topic overview: Voting phase
-- * 4.4 Topic overview: Result phase
-- * 4.5 Topic overview: Delegations
data ViewTopic
  = ViewTopicIdeas ViewTopicTab Topic [(Idea, Int)]
  | ViewTopicDelegations -- FIXME
  deriving (Eq, Show, Read)

instance Page ViewTopic where
    data PagePath ViewTopic = ViewTopicPath ViewTopicTab (AUID Topic)
    pagePath (ViewTopicPath _ _) = U.Broken -- TODO

-- | 10.1 Create topic: Create topic
data CreateTopic = CreateTopic IdeaSpace [AUID Idea]
  deriving (Eq, Show, Read)

instance Page CreateTopic where
    data PagePath CreateTopic = CreateTopicPath IdeaSpace (AUID Topic)
    pagePath (CreateTopicPath space topicId) =
        U.TopMain . U.Space space $ U.CreateTopicIdea topicId

-- | 10.2 Create topic: Move ideas to topic (Edit topic)
data MoveIdeasToTopic = MoveIdeasToTopic IdeaSpace (AUID Topic) [Idea]
  deriving (Eq, Show, Read)

instance Page MoveIdeasToTopic where
    data PagePath MoveIdeasToTopic = MoveIdeasToTopicPath IdeaSpace (AUID Topic)
    pagePath (MoveIdeasToTopicPath space topicId) =
        U.TopMain . U.Space space $ U.MoveIdeasToTopic topicId

-- | 10.3 ???
-- FIXME: Which page is this in the click-dummy?
data EditTopic = EditTopic
  deriving (Eq, Show, Read)

instance Page EditTopic where
    data PagePath EditTopic = EditTopicPath IdeaSpace (AUID Topic)
    pagePath (EditTopicPath space topicId) = U.TopMain . U.Space space $ U.EditTopic topicId

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
    toHtml p@ViewTopicDelegations = semanticDiv p "ViewTopicDelegations" -- FIXME
    toHtml p@(ViewTopicIdeas tab topic ideasAndNumVoters) = semanticDiv p $ do
        -- assert tab /= TabDelegation
        div_ $ do
            div_ [id_ "navigation"] $ do
                a_ [id_ "back-themes", href_ $ U.Space space U.ListTopics] "<- Zu Allen Themen"
                a_ [id_ "edit-topic",  href_ . U.Space space $ U.EditTopic topicId] $
                    span_ [id_ "pen"] ":pen:" <> " bearbeiten"
            h2_ . toHtml $ phaseName phase
            div_ $ do
                p_   [id_ "topic-title"] $ topic ^. topicTitle . html
                div_ [id_ "topic-desc"] $ topic ^. topicDesc . html
                when (phase == PhaseRefinement) $
                    a_ [id_ "add-idea", href_ . U.Space space $ U.CreateTopicIdea topicId] "+ Neue Idee"
                when (phase < PhaseResult) .
                    a_  [id_ "delegate-vote", href_ . U.Space space $ U.CreateTopicDelegation topicId] $
                        span_ [id_ "bullhorn"] ":bullhorn:" <> " Stimme Beauftragen"
            div_ [id_ "tabs"] $ do
                tabLink topic tab TabAllIdeas
                when (phase >= PhaseVoting) $ tabLink topic tab TabVotingIdeas
                when (phase >= PhaseResult) $ tabLink topic tab TabWinningIdeas
                tabLink topic tab TabDelegation
        div_ $ do
            a_ [id_ "settings"{-, href_ U.UserSettings FIXME USER??? -}] $ span_ [id_ "gear"] ":gear:"
            div_ [id_ "ideas"] . for_ ideasAndNumVoters $ \(idea, numVoters) ->
                ListItemIdea True (Just phase) numVoters idea ^. html
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

-- FIXME check the 'space'
viewTopic :: (ActionPersist r m, ActionUserHandler m, MonadError ActionExcept m)
    => IdeaSpace -> ViewTopicTab -> AUID Topic -> m (Frame ViewTopic)
viewTopic _space TabDelegation _ = makeFrame ViewTopicDelegations -- FIXME
viewTopic _space tab topicId = makeFrame =<< persistent (do
    -- FIXME 404
    Just topic <- findTopic topicId
    ViewTopicIdeas tab topic <$> (findIdeasByTopic topic >>= mapM getNumVotersForIdea))

createTopic :: (ActionM r action) => IdeaSpace -> [AUID Idea] -> ServerT (FormHandler CreateTopic) action
createTopic space ideas =
  redirectFormHandler (pure $ CreateTopic space ideas)
    (\protoTopic -> currentUser >>= persistent . flip addTopic protoTopic)

moveIdeasToTopic :: ActionM r m => IdeaSpace -> AUID Topic -> ServerT (FormHandler MoveIdeasToTopic) m
moveIdeasToTopic space topicId = redirectFormHandler getPage addIdeas
  where
    getPage = MoveIdeasToTopic space topicId <$> persistent (findWildIdeasBySpace space)
    addIdeas ideas = persistent $ Persistent.moveIdeasToLocation ideas (IdeaLocationTopic space topicId)

editTopic :: ActionM r m => IdeaSpace -> AUID Topic -> m (Frame EditTopic)
editTopic _ _ = makeFrame EditTopic

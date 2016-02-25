{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Page.Topic
    ( ViewTopic(..)
    , ViewTopicTab(..)
    , CreateTopic(..)
    , MoveIdeasToTopic(..)
    , viewTopic
    , createTopic
    , moveIdeasToTopic )
where

import Action (ActionM, ActionPersist(..))
import Frontend.Prelude hiding (moveIdeasToTopic)

import qualified Api.Persistent as Persistent
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Lucid.Html5 as DF

----------------------------------------------------------------------
-- types

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
  = ViewTopicIdeas ViewTopicTab Topic [Idea]
  | ViewTopicDelegations -- FIXME
  deriving (Eq, Show, Read)

instance Page ViewTopic where
    isPrivatePage _ = True

-- | 10.1 Create topic: Create topic
data CreateTopic = CreateTopic IdeaSpace [AUID Idea]
  deriving (Eq, Show, Read)

instance Page CreateTopic where
    isPrivatePage _ = True

-- | 10.2 Create topic: Move ideas to topic
data MoveIdeasToTopic = MoveIdeasToTopic (AUID Topic) [Idea]
  deriving (Eq, Show, Read)

instance Page MoveIdeasToTopic where
    isPrivatePage _ = True


----------------------------------------------------------------------
-- templates

tabSelected :: Eq tab => tab -> tab -> ST
tabSelected curTab targetTab
    | curTab == targetTab = "tab-selected"
    | otherwise           = "tab-not-selected"

tabLink :: Monad m => ViewTopicTab -> ViewTopicTab -> HtmlT m ()
tabLink curTab targetTab =
  case targetTab of
    TabAllIdeas     -> a_ [id_ "tab-ideas",       attr] "Alle Ideen"
    TabVotingIdeas  -> a_ [id_ "tab-voting",      attr] "Ideen in der Abstimmung"
    TabWinningIdeas -> a_ [id_ "tab-winning",     attr] "Gewinner"
    TabDelegation   -> a_ [id_ "tab-delegations", attr] "Beauftragen Stimmen"
  where
    attr = class_ $ tabSelected curTab targetTab

-- FIXME: how do we display a topic in the finished phase?
-- Is this the same the result phase?
-- Maybe some buttons to hide?
instance ToHtml ViewTopic where
    toHtmlRaw = toHtml
    toHtml p@ViewTopicDelegations = semanticDiv p "ViewTopicDelegations" -- FIXME
    toHtml p@(ViewTopicIdeas tab topic ideas) = semanticDiv p $ do
        -- assert tab /= TabDelegation
        div_ $ do
            div_ [id_ "navigation"] $ do
                a_ [id_ "back-themes"] "<- Zu Allen Themen"
                a_ $ span_ [id_ "pen"] ":pen:" <> " bearbeiten"
            h2_ . toHtml $ phaseName phase
            div_ $ do
                p_   [id_ "topic-title"] $ topic ^. topicTitle . html
                div_ [id_ "topic-desc"] $ topic ^. topicDesc . html
                when (phase == PhaseRefinement) $
                    a_   [id_ "add-idea"] "+ Neue Idee"
                when (phase < PhaseResult) .
                    a_  [id_ "delegate-vote"] $ span_ [id_ "bullhorn"] ":bullhorn:" <> " Stimme Beauftragen"
            div_ [id_ "tabs"] $ do
                tabLink tab TabAllIdeas
                when (phase >= PhaseVoting) $ tabLink tab TabVotingIdeas
                when (phase >= PhaseResult) $ tabLink tab TabWinningIdeas
                tabLink tab TabDelegation
        div_ $ do
            a_ [id_ "settings"] $ span_ [id_ "gear"] ":gear:"
            div_ [id_ "ideas"] . for_ ideas $ \idea ->
                ListItemIdea (Just phase) idea ^. html
      where
        phase = topic ^. topicPhase

instance ToHtml CreateTopic where
    toHtmlRaw = toHtml
    toHtml p = semanticDiv p $ do
        p_ "The topic has been created." >> br_ []
        p_ "Fügen Sie weitere wilde ideen dem neuen Thema hinzu"
        a_ [id_ "add-ideas"] "+ Ideen auswählen"

instance FormPageView CreateTopic where
    type FormPageResult CreateTopic = ProtoTopic

    formAction _ = "/topics/create"

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

instance FormPageView MoveIdeasToTopic where
    -- While the input page contains all the wild ideas the result page only contains
    -- the ideas to be added to the topic.
    type FormPageResult MoveIdeasToTopic = [AUID Idea]

    formAction (MoveIdeasToTopic topicId _) =
        "/topics/" <> cs (show topicId) <> "/ideas"

    makeForm (MoveIdeasToTopic _ ideas) =
        fmap catMaybes . sequenceA $
        [ justIf (idea ^. _Id) <$> (ideaToFormField idea .: DF.bool Nothing) | idea <- ideas ]

    formPage v fa p@(MoveIdeasToTopic _ ideas) = do
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

----------------------------------------------------------------------
-- redirects

instance RedirectOf CreateTopic where
    redirectOf _ = "/topics"

instance RedirectOf MoveIdeasToTopic where
    redirectOf (MoveIdeasToTopic topicId _) = "/topics/" <> cs (show topicId) -- FIXME safe links


----------------------------------------------------------------------
-- handlers

viewTopic :: ActionPersist m => ViewTopicTab -> AUID Topic -> m (Frame ViewTopic)
viewTopic TabDelegation _ = pure . makeFrame $ ViewTopicDelegations -- FIXME
viewTopic tab topicId = persistent $ do
    -- FIXME 404
    Just topic <- findTopic topicId
    ideas      <- findIdeasByTopic topic
    pure . makeFrame $ ViewTopicIdeas tab topic ideas

createTopic :: (ActionM action) => IdeaSpace -> [AUID Idea] -> ServerT (FormH HTML (Html ()) ST) action
createTopic space ideas = redirectFormHandler (pure $ CreateTopic space ideas) (persistent . addTopic)

moveIdeasToTopic :: ActionM m => IdeaSpace -> AUID Topic -> ServerT (FormH HTML (Html ()) ST) m
moveIdeasToTopic space topicId = redirectFormHandler getPage addIdeas
  where
    getPage = MoveIdeasToTopic topicId <$> persistent (findWildIdeasBySpace space)
    addIdeas ideas = persistent $ Persistent.moveIdeasToTopic ideas (Just topicId)

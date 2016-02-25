{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Werror #-}

module Frontend.Page.Idea
  ( ViewIdea(..)
  , CreateIdea(..)
  , EditIdea(..)
  , viewIdea
  , createIdea
  , editIdea
  , categoryValues
  )
where

import Action (ActionM, ActionPersist, persistent)
import Frontend.Page.Comment
import Frontend.Path (Top(TopTesting), path)
import Frontend.Prelude

import qualified Data.Set as Set
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Lucid.Html5 as DF

----------------------------------------------------------------------
-- types

-- | 5 Idea detail page
-- This includes the pages 5.1 to 5.7 excluding 5.5 (PageIdeaDetailMoveIdeaToTopic) which needs its
-- own endpoint.
--
-- * 5.1 Idea detail page: New ideas
-- * 5.2 Idea detail page: Refinement phase
-- * 5.3 Idea detail page: Jury (assessment) phase
-- * 5.4 Idea detail page: Voting phase
-- * 5.6 Idea detail page: Feasible / not feasible
-- * 5.7 Idea detail page: Winner
data ViewIdea = ViewIdea Idea (Maybe Phase)
  deriving (Eq, Show, Read)

instance Page ViewIdea where
  isPrivatePage _ = True

-- | 6. Create idea
data CreateIdea = CreateIdea
  deriving (Eq, Show, Read)

instance Page CreateIdea where
  isPrivatePage _ = True

-- | 7. Edit idea
data EditIdea = EditIdea Idea
  deriving (Eq, Show, Read)

instance Page EditIdea where
  isPrivatePage _ = True


----------------------------------------------------------------------
-- templates

instance ToHtml ViewIdea where
    toHtmlRaw = toHtml
    -- NP: I've avoided here complex conditionals.
    -- The result might be that too much information is displayed.
    toHtml p@(ViewIdea idea phase) = semanticDiv p $ do
        h2_ $ idea ^. ideaTitle . html

        div_ [id_ "author"]   $ idea ^. ideaMeta . to AuthorWidget . html
        div_ [id_ "category"] $ idea ^. ideaCategory . showed . html

        div_ [id_ "badges"] $ do
            -- At most one badge should be displayed
            when (notFeasibleIdea idea) $ span_ [id_ "cross-mark"] ":cross-mark:"
            when (winningIdea idea)     $ span_ [id_ "medal"] ":medal:"

        -- von X / X stimmen / X verbesserungvorschläge
        when (phase >= Just PhaseVoting) . div_ [id_ "votes"] $ do
            span_ $ "von " <> idea ^. createdBy . showed . html
            span_ "/"
            span_ $ totalVotes ^. showed . html <> " Stimmen"
            span_ "/"
            span_ $ totalComments ^. showed . html <> " Verbesserungsvorschläge"

        -- visual vote stats
        when (phase >= Just PhaseVoting) . div_ [id_ "votes-stats"] . pre_ $ do
            let y = countVotes Yes ideaVoteValue $ idea ^. ideaVotes
                n = countVotes No  ideaVoteValue $ idea ^. ideaVotes
            div_ $ do
                span_ . toHtml $ "    " <> replicate y '+' <> ":" <> replicate n '-'
            div_ $ do
                span_ . toHtml $ replicate (4 + y - length (show y)) ' ' <> show y <> ":" <> show n

        -- buttons
        when (phase == Just PhaseVoting) . div_ [id_ "voting"] $ do
            button_ [value_ "yes"]     "dafür"
            button_ [value_ "neutral"] "neutral"
            button_ [value_ "no"]      "dagegen"

        -- article
        div_ [id_ "desc"] $ idea ^. ideaDesc . html

        -- comments
        div_ [id_ "comments"] $ do
            hr_ []
            span_ $ totalComments ^. showed . html <> " Verbesserungsvorschläge"
            span_ $ button_ [value_ "create_comment"] "Neuer Verbesserungsvorschlag"
            hr_ []
            for_ (idea ^. ideaComments) $ \c ->
                PageComment c ^. html
      where
        totalVotes    = Set.size $ idea ^. ideaVotes
        totalComments = Set.size $ idea ^. ideaComments


instance FormPageView CreateIdea where
    type FormPageResult CreateIdea = ProtoIdea

    formAction _ = path $ TopTesting "/ideas/create"

    makeForm _ =
        ProtoIdea
        <$> ("title"         .: DF.text Nothing)
        <*> ("idea-text"     .: (Markdown <$> DF.text Nothing))
        <*> ("idea-category" .: DF.choice categoryValues Nothing)

    formPage v fa p = do
        semanticDiv p $ do
            h3_ "Create Idee"
            DF.form v fa $ do
                ideaFormFields v
                DF.inputSubmit   "Add Idea"

instance FormPageView EditIdea where
    type FormPageResult EditIdea = ProtoIdea

    formAction (EditIdea idea) =
        path . TopTesting $ "/ideas/edit/" <> cs (show . (\ (AUID i) -> i) $ idea ^. _Id)

    makeForm (EditIdea idea) =
        ProtoIdea
        <$> ("title"         .: DF.text (Just $ idea ^. ideaTitle))
        <*> ("idea-text"     .: (Markdown <$> DF.text (Just . fromMarkdown $ idea ^. ideaDesc)))
        <*> ("idea-category" .: DF.choice categoryValues (Just $ idea ^. ideaCategory))

    formPage v fa p@(EditIdea _idea) =
        semanticDiv p $ do
            h3_ "Diene Idee"
            DF.form v fa $ do
                ideaFormFields v
                DF.inputSubmit   "Sriechern"
                button_ [value_ ""] "IDEE LOSCHEN"
                button_ [value_ ""] "Abbrechen"

categoryValues :: IsString s => [(Category, s)]
categoryValues = [ (CatRule,        "Regel")
                 , (CatEquipment,   "Ausstattung")
                 , (CatClass,       "Unterricht")
                 , (CatTime,        "Zeit")
                 , (CatEnvironment, "Umgebung")
                 ]

ideaFormFields :: Monad m => View (HtmlT m ()) -> HtmlT m ()
ideaFormFields v = do
    DF.inputText     "title" v >> br_ []
    DF.inputTextArea Nothing Nothing "idea-text" v >> br_ []
    DF.inputSelect   "idea-category" v >> br_ []


----------------------------------------------------------------------
-- handlers

-- FIXME restrict to the given IdeaSpace
viewIdea :: ActionPersist m => IdeaSpace -> AUID Idea -> m (Frame ViewIdea)
viewIdea _space ideaId = persistent $ do
    -- FIXME 404
    Just idea  <- findIdea ideaId
    phase <-
        case idea ^. ideaTopic of
            Nothing ->
                pure Nothing
            Just topicId -> do
                -- FIXME 404
                Just topic <- findTopic topicId
                pure . Just $ topic ^. topicPhase
    pure . makeFrame $ ViewIdea idea phase

-- FIXME: Redirect to the right place
instance RedirectOf CreateIdea where
    redirectOf _ = path $ TopTesting "/ideas"

createIdea :: (ActionM action) => ServerT (FormH HTML (Html ()) ST) action
createIdea = redirectFormHandler (pure CreateIdea) (persistent . addIdea)

-- FIXME: Redirect to the right place
instance RedirectOf EditIdea where
    redirectOf _ = path $ TopTesting "/ideas"

editIdea :: (ActionM action) => AUID Idea -> ServerT (FormH HTML (Html ()) ST) action
editIdea ideaId =
    redirectFormHandler
        (EditIdea . (\ (Just idea) -> idea) <$> persistent (findIdea ideaId))
        (persistent . modifyIdea ideaId . newIdea)
  where
    newIdea protoIdea =   (ideaTitle .~ (protoIdea ^. protoIdeaTitle))
                        . (ideaDesc .~ (protoIdea ^. protoIdeaDesc))
                        . (ideaCategory .~ (protoIdea ^. protoIdeaCategory))

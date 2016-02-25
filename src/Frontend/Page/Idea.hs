{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Werror #-}

module Frontend.Page.Idea
  ( PageIdeaDetail(..)
  , PageIdea(..)
  , PageIdeaDetailNewIdeas(..)
  , PageIdeaDetailRefinementPhase(..)
  , PageIdeaDetailJuryPhase(..)
  , PageIdeaDetailVotingPhase(..)
  , PageIdeaDetailFeasibleNotFeasible(..)
  , PageIdeaDetailWinner(..)
  , PageCreateIdea(..)
  , PageEditIdea(..)
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
-- page

-- | 5 Idea detail page
-- This includes the pages 5.1 to 5.7 excluding 5.5 (PageIdeaDetailMoveIdeaToTopic) which needs its
-- own endpoint.
data PageIdeaDetail
  = PageIdeaDetailNewIdeas'            PageIdeaDetailNewIdeas
  | PageIdeaDetailRefinementPhase'     PageIdeaDetailRefinementPhase
  | PageIdeaDetailJuryPhase'           PageIdeaDetailJuryPhase
  | PageIdeaDetailVotingPhase'         PageIdeaDetailVotingPhase
  | PageIdeaDetailFeasibleNotFeasible' PageIdeaDetailFeasibleNotFeasible
  | PageIdeaDetailWinner'              PageIdeaDetailWinner
    deriving (Eq, Show, Read)

instance ToHtml PageIdeaDetail where
    toHtmlRaw = toHtml
    toHtml = \case
      PageIdeaDetailNewIdeas'            p -> toHtml p
      PageIdeaDetailRefinementPhase'     p -> toHtml p
      PageIdeaDetailJuryPhase'           p -> toHtml p
      PageIdeaDetailVotingPhase'         p -> toHtml p
      PageIdeaDetailFeasibleNotFeasible' p -> toHtml p
      PageIdeaDetailWinner'              p -> toHtml p

pageIdeaDetailPhase :: Idea -> Maybe Phase -> PageIdeaDetail
pageIdeaDetailPhase idea = \case
    _ | notFeasibleIdea idea -> PageIdeaDetailFeasibleNotFeasible' . PageIdeaDetailFeasibleNotFeasible $ idea
      | winningIdea     idea -> PageIdeaDetailWinner'              . PageIdeaDetailWinner              $ idea
    Nothing                  -> PageIdeaDetailNewIdeas'            . PageIdeaDetailNewIdeas            $ idea
    Just PhaseRefinement     -> PageIdeaDetailRefinementPhase'     . PageIdeaDetailRefinementPhase     $ idea
    Just PhaseJury           -> PageIdeaDetailJuryPhase'           . PageIdeaDetailJuryPhase           $ idea
    Just PhaseVoting         -> PageIdeaDetailVotingPhase'         . PageIdeaDetailVotingPhase         $ idea
    -- FIXME: how do we display an idea which is *not winning* and potentially *feasible* in the
    -- result and finished phases?
    -- Is this the same the voting phase?
    -- Maybe some buttons to hide?
    Just PhaseResult         -> PageIdeaDetailVotingPhase'         . PageIdeaDetailVotingPhase         $ idea
    Just PhaseFinished       -> PageIdeaDetailVotingPhase'         . PageIdeaDetailVotingPhase         $ idea

-- FIXME restrict to the given IdeaSpace
viewIdea :: ActionPersist m => IdeaSpace -> AUID Idea -> m (Frame PageIdeaDetail)
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
    pure . Frame frameUserHack $ pageIdeaDetailPhase idea phase

-- NP: I've avoided here complex conditionals.
-- The result might be that too much information is displayed.
pageIdeaDetailTemplate :: Monad m => Idea -> Maybe Phase -> HtmlT m ()
pageIdeaDetailTemplate idea phase = do
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


data PageIdea = PageIdea Idea
  deriving (Eq, Show, Read)

instance ToHtml PageIdea where
    toHtmlRaw = toHtml
    toHtml p@(PageIdea idea) = semanticDiv p $ pageIdeaDetailTemplate idea Nothing


-- | 5.1 Idea detail page: New ideas
data PageIdeaDetailNewIdeas = PageIdeaDetailNewIdeas Idea
    deriving (Eq, Show, Read)

instance ToHtml PageIdeaDetailNewIdeas where
    toHtmlRaw = toHtml
    toHtml p@(PageIdeaDetailNewIdeas idea) = semanticDiv p $ toHtml (PageIdea idea)


-- | 5.2 Idea detail page: Refinement phase
data PageIdeaDetailRefinementPhase = PageIdeaDetailRefinementPhase Idea
    deriving (Eq, Show, Read)

instance ToHtml PageIdeaDetailRefinementPhase where
    toHtmlRaw = toHtml
    toHtml p@(PageIdeaDetailRefinementPhase idea) = semanticDiv p $ pageIdeaDetailTemplate idea (Just PhaseRefinement)


-- | 5.3 Idea detail page: Jury (assessment) phase
data PageIdeaDetailJuryPhase = PageIdeaDetailJuryPhase Idea
    deriving (Eq, Show, Read)

instance ToHtml PageIdeaDetailJuryPhase where
    toHtmlRaw = toHtml
    toHtml p@(PageIdeaDetailJuryPhase idea) = semanticDiv p $ pageIdeaDetailTemplate idea (Just PhaseJury)


-- | 5.4 Idea detail page: Voting phase
data PageIdeaDetailVotingPhase = PageIdeaDetailVotingPhase Idea
    deriving (Eq, Show, Read)

instance ToHtml PageIdeaDetailVotingPhase where
    toHtmlRaw = toHtml
    toHtml p@(PageIdeaDetailVotingPhase idea) = semanticDiv p $ pageIdeaDetailTemplate idea (Just PhaseVoting)


-- | 5.6 Idea detail page: Feasible / not feasible
data PageIdeaDetailFeasibleNotFeasible = PageIdeaDetailFeasibleNotFeasible Idea
    deriving (Eq, Show, Read)

instance ToHtml PageIdeaDetailFeasibleNotFeasible where
    toHtmlRaw = toHtml
    toHtml p@(PageIdeaDetailFeasibleNotFeasible idea) = semanticDiv p $ pageIdeaDetailTemplate idea Nothing


-- | 5.7 Idea detail page: Winner
data PageIdeaDetailWinner = PageIdeaDetailWinner Idea
    deriving (Eq, Show, Read)

instance ToHtml PageIdeaDetailWinner where
    toHtmlRaw = toHtml
    toHtml p = semanticDiv p "PageIdeaDetailWinner"


-- | 6. Create idea
data PageCreateIdea = PageCreateIdea
  deriving (Eq, Show, Read)

instance Page PageCreateIdea where
  isPrivatePage _ = True

-- | 7. Edit idea
data PageEditIdea = PageEditIdea Idea
  deriving (Eq, Show, Read)

instance Page PageEditIdea where
  isPrivatePage _ = True

----------------------------------------------------------------------
-- templates

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

instance FormPageView PageCreateIdea where
    type FormPageResult PageCreateIdea = ProtoIdea

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

instance FormPageView PageEditIdea where
    type FormPageResult PageEditIdea = ProtoIdea

    formAction (PageEditIdea idea) =
        path . TopTesting $ "/ideas/edit/" <> cs (show . (\ (AUID i) -> i) $ idea ^. _Id)

    makeForm (PageEditIdea idea) =
        ProtoIdea
        <$> ("title"         .: DF.text (Just $ idea ^. ideaTitle))
        <*> ("idea-text"     .: (Markdown <$> DF.text (Just . fromMarkdown $ idea ^. ideaDesc)))
        <*> ("idea-category" .: DF.choice categoryValues (Just $ idea ^. ideaCategory))

    formPage v fa p@(PageEditIdea _idea) =
        semanticDiv p $ do
            h3_ "Diene Idee"
            DF.form v fa $ do
                ideaFormFields v
                DF.inputSubmit   "Sriechern"
                button_ [value_ ""] "IDEE LOSCHEN"
                button_ [value_ ""] "Abbrechen"


----------------------------------------------------------------------
-- handlers

-- FIXME: Redirect to the right place
instance RedirectOf PageCreateIdea where
    redirectOf _ = path $ TopTesting "/ideas"

createIdea :: (ActionM action) => ServerT (FormH HTML (Html ()) ST) action
createIdea = redirectFormHandler (pure PageCreateIdea) (persistent . addIdea)

-- FIXME: Redirect to the right place
instance RedirectOf PageEditIdea where
    redirectOf _ = path $ TopTesting "/ideas"

editIdea :: (ActionM action) => AUID Idea -> ServerT (FormH HTML (Html ()) ST) action
editIdea ideaId =
    redirectFormHandler
        (PageEditIdea . (\ (Just idea) -> idea) <$> persistent (findIdea ideaId))
        (persistent . modifyIdea ideaId . newIdea)
  where
    newIdea protoIdea =   (ideaTitle .~ (protoIdea ^. protoIdeaTitle))
                        . (ideaDesc .~ (protoIdea ^. protoIdeaDesc))
                        . (ideaCategory .~ (protoIdea ^. protoIdeaCategory))

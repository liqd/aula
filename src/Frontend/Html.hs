{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}

{-# OPTIONS_GHC -Werror -Wall #-}

-- | ...
--
-- We provide data types @Page...@ even if there is an application type already.  Example: For
-- 'Idea', we define 'PageIdea'.  This has at least two benefits:
--
-- - page types should always be defined here to avoid orphans;
-- - we can add additional information (like author name if we only have an author's id) and thus
--   avoid making page rendering effectful.
module Frontend.Html
where

import Control.Lens
import Control.Monad
import Data.Foldable (for_)
import Data.String.Conversions
import Prelude
import Lucid hiding (for_)

import qualified Data.Set as Set

import Action (ActionM, persistent)
import Api.Persistent (findIdea, findTopic)
import Api
import Types
import Frontend.Core


----------------------------------------------------------------------
-- pages

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

pageIdeaDetail :: ActionM m => AUID Idea -> m (Frame PageIdeaDetail)
pageIdeaDetail ideaId = persistent $ do
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


-- | 5.5 Idea detail page: Move idea to topic
data PageIdeaDetailMoveIdeaToTopic = PageIdeaDetailMoveIdeaToTopic
    deriving (Eq, Show, Read)

instance ToHtml PageIdeaDetailMoveIdeaToTopic where
    toHtmlRaw = toHtml
    toHtml p = semanticDiv p "PageIdeaDetailMoveIdeaToTopic"


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


-- | 7. Edit idea
data PageEditIdea = PageEditIdea
  deriving (Eq, Show, Read)

instance ToHtml PageEditIdea where
    toHtmlRaw = toHtml
    toHtml p = semanticDiv p "PageEditIdea"


-- | 8.1 User profile: Created ideas
data PageUserProfileCreateIdeas = PageUserProfileCreateIdeas
  deriving (Eq, Show, Read)

instance ToHtml PageUserProfileCreateIdeas where
    toHtmlRaw = toHtml
    toHtml p = semanticDiv p "PageUserProfileCreateIdeas"


-- | 8.2 User profile: Delegated votes
data PageUserProfileDelegatedVotes = PageUserProfileDelegatedVotes
  deriving (Eq, Show, Read)

instance ToHtml PageUserProfileDelegatedVotes where
    toHtmlRaw = toHtml
    toHtml p = semanticDiv p "PageUserProfileDelegatedVotes"


-- | 9. User settings
data PageUserSettings = PageUserSettings
  deriving (Eq, Show, Read)

instance ToHtml PageUserSettings where
    toHtmlRaw = toHtml
    toHtml p = semanticDiv p "PageUserSettings"


-- | 11.1 Admin settings: Durations & quorum
data PageAdminSettingsDurationsAndQuorum =
    PageAdminSettingsDurationsAndQuorum
  deriving (Eq, Show, Read)

instance ToHtml PageAdminSettingsDurationsAndQuorum where
    toHtmlRaw = toHtml
    toHtml p = semanticDiv p "PageAdminSettingsDurationsAndQuorum"


-- | 11.2 Admin settings: Manage groups & permissions
data PageAdminSettingsGroupsAndPermissions =
    PageAdminSettingsGroupsAndPermissions
  deriving (Eq, Show, Read)

instance ToHtml PageAdminSettingsGroupsAndPermissions where
    toHtmlRaw = toHtml
    toHtml p = semanticDiv p "PageAdminSettingsGroupsAndPermissions"


-- | 11.3 Admin settings: User creation & user import
data PageAdminSettingsUserCreateAndImport =
    PageAdminSettingsUserCreateAndImport
  deriving (Eq, Show, Read)

instance ToHtml PageAdminSettingsUserCreateAndImport where
    toHtmlRaw = toHtml
    toHtml p = semanticDiv p "PageAdminSettingsUserCreateAndImport"


-- | 11.4 Admin settings: Events protocol
data PageAdminSettingsEventsProtocol =
    PageAdminSettingsEventsProtocol
  deriving (Eq, Show, Read)

instance ToHtml PageAdminSettingsEventsProtocol where
    toHtmlRaw = toHtml
    toHtml p = semanticDiv p "PageAdminSettingsEventsProtocol"


-- | 12. Delegate vote
data PageDelegateVote = PageDelegateVote
  deriving (Eq, Show, Read)

instance ToHtml PageDelegateVote where
    toHtmlRaw = toHtml
    toHtml p = semanticDiv p "PageDelegateVote"


-- | 13. Delegation network
data PageDelegationNetwork = PageDelegationNetwork
  deriving (Eq, Show, Read)

instance ToHtml PageDelegationNetwork where
    toHtmlRaw = toHtml
    toHtml p = semanticDiv p "PageDelegationNetwork"


data PageIdea = PageIdea Idea
  deriving (Eq, Show, Read)

instance ToHtml PageIdea where
    toHtmlRaw = toHtml
    toHtml p@(PageIdea idea) = semanticDiv p $ pageIdeaDetailTemplate idea Nothing

data PageComment = PageComment Comment
  deriving (Eq, Show, Read)

instance ToHtml PageComment where
    toHtmlRaw = toHtml
    toHtml p@(PageComment comment) = semanticDiv p $ do
        div_ $ do
            span_ $ comment ^. commentMeta . to AuthorWidget . html
            span_ $ comment ^. commentVotes . to VotesWidget . html
        div_ $ do
            comment ^. commentText . html
        div_ $ do
            span_ "[antworten]"
            span_ "[melden]"

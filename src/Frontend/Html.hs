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
import Control.Monad (forM_)
import Data.Foldable (for_)
import Data.String.Conversions
import Prelude
import Lucid hiding (for_)

import qualified Data.Set as Set

import Api
import Types
import Frontend.Core


----------------------------------------------------------------------
-- pages

-- | 1. Rooms overview
data PageRoomsOverview = PageRoomsOverview [String]
  deriving (Eq, Show, Read)

instance ToHtml PageRoomsOverview where
    toHtmlRaw = toHtml
    toHtml p@(PageRoomsOverview rooms) = semanticDiv p $ case rooms of
      [] -> p_ "Keine Ideenräume"
      _  -> forM_ rooms $ div_ . toHtml


-- | 2. Ideas overview
data PageIdeasOverview = PageIdeasOverview [Idea]
  deriving (Eq, Show, Read)

instance ToHtml PageIdeasOverview where
    toHtmlRaw = toHtml
    toHtml p@(PageIdeasOverview ideas) = semanticDiv p $ do
        p_ "WILDE IDEEN"
        h1_ "Was soll sich verändern?"
        p_ $ "Du kannst hier jede lose Idee, die du im Kopf hast, einwerfen und kannst fuer die "
            <> "Idee abstimmen und diese somit \"auf den Tisch bringen\"."
        div_ $ button_ [onclick_ "location.href='/ideas/create'"] "+ Neue Idee" -- FIXME: should link to idea creation form
        div_ $ do
            -- FIXME: these buttons should filter the ideas by category
            button_ "Regeln"
            button_ "Ausstattung"
            button_ "Unterricht"
            button_ "Zeit"
            button_ "Umgebung"
        div_ [id_ "ideas"] . for_ ideas $ \idea ->
            ListItemIdea Nothing idea ^. html

instance Page PageIdeasOverview where
    isPrivatePage _ = True

-- | 3. Ideas in discussion
data PageIdeasInDiscussion = PageIdeasInDiscussion
  deriving (Eq, Show, Read)

instance ToHtml PageIdeasInDiscussion where
    toHtmlRaw = toHtml
    toHtml p = semanticDiv p "PageIdeasInDiscussion"


-- | 5.1 Idea detail page: New ideas
data PageIdeaDetailNewIdeas = PageIdeaDetailNewIdeas Idea
    deriving (Eq, Show, Read)

instance ToHtml PageIdeaDetailNewIdeas where
    toHtmlRaw = toHtml
    toHtml p@(PageIdeaDetailNewIdeas idea) = semanticDiv p $ toHtml (PageIdea idea)


-- | 5.2 Idea detail page: Refinement phase
data PageIdeaDetailRefinementPhase = PageIdeaDetailRefinementPhase
    deriving (Eq, Show, Read)

instance ToHtml PageIdeaDetailRefinementPhase where
    toHtmlRaw = toHtml
    toHtml p = semanticDiv p "PageIdeaDetailRefinementPhase"


-- | 5.3 Idea detail page: Jury (assessment) phase
data PageIdeaDetailJuryPhase = PageIdeaDetailJuryPhase
    deriving (Eq, Show, Read)

instance ToHtml PageIdeaDetailJuryPhase where
    toHtmlRaw = toHtml
    toHtml p = semanticDiv p "PageIdeaDetailJuryPhase"


-- | 5.4 Idea detail page: Voting phase
data PageIdeaDetailVotingPhase = PageIdeaDetailVotingPhase
    deriving (Eq, Show, Read)

instance ToHtml PageIdeaDetailVotingPhase where
    toHtmlRaw = toHtml
    toHtml p = semanticDiv p "PageIdeaDetailVotingPhase"


-- | 5.5 Idea detail page: Move idea to topic
data PageIdeaDetailMoveIdeaToTopic = PageIdeaDetailMoveIdeaToTopic
    deriving (Eq, Show, Read)

instance ToHtml PageIdeaDetailMoveIdeaToTopic where
    toHtmlRaw = toHtml
    toHtml p = semanticDiv p "PageIdeaDetailMoveIdeaToTopic"


-- | 5.6 Idea detail page: Feasible / not feasible
data PageIdeaDetailFeasibleNotFeasible = PageIdeaDetailFeasibleNotFeasible
    deriving (Eq, Show, Read)

instance ToHtml PageIdeaDetailFeasibleNotFeasible where
    toHtmlRaw = toHtml
    toHtml p = semanticDiv p "PageIdeaDetailFeasibleNotFeasible"


-- | 5.7 Idea detail page: Winner
data PageIdeaDetailWinner = PageIdeaDetailWinner
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
    toHtml p@(PageIdea idea) = semanticDiv p $ do
        let totalVotes    = Set.size $ idea ^. ideaVotes
            totalComments = Set.size $ idea ^. ideaComments
        h2_ $ idea ^. ideaTitle . html

        div_ $ idea ^. ideaMeta . to AuthorWidget . html
        div_ $ idea ^. ideaCategory . showed . html

        -- von X / X stimmen / X verbesserungvorschläge
        div_ $ do
            span_ $ "von " <> idea ^. createdBy . showed . html
            span_ "/"
            span_ $ totalVotes ^. showed . html <> " Stimmen"
            span_ "/"
            span_ $ totalComments ^. showed . html <> " Verbesserungsvorschläge"

        -- visual vote stats
        div_ . pre_ $ do
            let y = countVotes Yes ideaVoteValue $ idea ^. ideaVotes
                n = countVotes No  ideaVoteValue $ idea ^. ideaVotes
            div_ $ do
                span_ . toHtml $ "    " <> replicate y '+' <> ":" <> replicate n '-'
            div_ $ do
                span_ . toHtml $ replicate (4 + y - length (show y)) ' ' <> show y <> ":" <> show n

        -- buttons
        div_ $ do
            button_ [value_ "yes"]     "dafür"
            button_ [value_ "neutral"] "neutral"
            button_ [value_ "no"]      "dagegen"

        -- article
        div_ $ idea ^. ideaDesc . html

        -- comments
        div_ $ do
            hr_ []
            span_ $ totalComments ^. showed . html <> " Verbesserungsvorschläge"
            span_ $ button_ [value_ "create_comment"] "Neuer Verbesserungsvorschlag"
            hr_ []
            for_ (idea ^. ideaComments) $ \c ->
                PageComment c ^. html

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

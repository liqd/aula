{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Werror #-}

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

import Control.Lens ((^.))
import Data.Set (Set)
import Data.String.Conversions
import Prelude hiding (head, span, div)
import Text.Blaze
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A hiding (span, title)

import qualified Data.Set as Set

import Api
import Types


----------------------------------------------------------------------
-- building blocks

-- | Wrap anything that has 'ToMarkup' and wrap it in an HTML body with complete page.
newtype Frame body = Frame body

instance (ToMarkup body) => ToMarkup (Frame body) where
    toMarkup (Frame bdy) = do
        head $ do
            title $ text "AuLA"
            link ! rel "stylesheet" ! href "/screen.css"
        body (toMarkup bdy)

newtype CommentVotesWidget = VotesWidget (Set CommentVote)

instance ToMarkup CommentVotesWidget where
    toMarkup (VotesWidget votes) = string $ y ++ n
      where
        y = "[up: "   <> show (countVotes Up   commentVoteValue votes) <> "]"
        n = "[down: " <> show (countVotes Down commentVoteValue votes) <> "]"

newtype AuthorWidget a = AuthorWidget (MetaInfo a)

instance ToMarkup (AuthorWidget a) where
    toMarkup (AuthorWidget mi) = span $ do
        text "["
        img ! (src . textValue $ mi ^. metaCreatedByAvatar)
        text (mi ^. metaCreatedByLogin)
        text "]"


----------------------------------------------------------------------
-- pages

-- | 1. Rooms overview
data PageRoomsOverview = PageRoomsOverview
  deriving (Eq, Show, Read)

instance ToMarkup PageRoomsOverview where
    toMarkup _ = "PageRoomsOverview"


-- | 2. Ideas overview
data PageIdeasOverview = PageIdeasOverview
  deriving (Eq, Show, Read)

instance ToMarkup PageIdeasOverview where
    toMarkup _ = "PageIdeasOverview"


-- | 3. Ideas in discussion
data PageIdeasInDiscussion = PageIdeasInDiscussion
  deriving (Eq, Show, Read)

instance ToMarkup PageIdeasInDiscussion where
    toMarkup _ = "PageIdeasInDiscussion"


-- | 4. Topic overview
data PageTopicOverview (a :: TopicOverviewPageState) = PageTopicOverview
  deriving (Eq, Show, Read)

data TopicOverviewPageState =
    TopicOverviewRefinementPhase   -- ^ 4.1 Topic overview: Refinement phase
  | TopicOverviewAssessmentPhase   -- ^ 4.2 Topic overview: Assessment phase
  | TopicOverviewVotingPhase       -- ^ 4.3 Topic overview: Voting phase
  | TopicOverviewResultPhase       -- ^ 4.4 Topic overview: Result phase
  | TopicOverviewDelegations       -- ^ 4.5 Topic overview: Delegations
  deriving (Eq, Show, Enum, Bounded, Read)

instance ToMarkup (PageTopicOverview a) where
    toMarkup _ = "PageTopicOverview TopicOverviewPageState"


-- | 5. Idea detail page
data PageIdeaDetail (a :: IdeaDetailPageState) = PageIdeaDetail Idea
  deriving (Eq, Show, Read)

data IdeaDetailPageState =
    IdeaDetailNewIdeas             -- ^ 5.1 Idea detail page: New ideas
  | IdeaDetailRefinementPhase      -- ^ 5.2 Idea detail page: Refinement phase
  | IdeaDetailAssessmentPhase      -- ^ 5.3 Idea detail page: Assessment phase
  | IdeaDetailVotingPhase          -- ^ 5.4 Idea detail page: Voting phase
  | IdeaDetailMoveIdeaToTopic      -- ^ 5.5 Idea detail page: Move idea to topic
  | IdeaDetailFeasibleNotFeasible  -- ^ 5.6 Idea detail page: Feasible / not feasible
  | IdeaDetailWinner               -- ^ 5.7 Idea detail page: Winner
  deriving (Eq, Show, Enum, Bounded, Read)

instance ToMarkup (PageIdeaDetail a) where
    toMarkup (PageIdeaDetail idea) = toMarkup (PageIdea idea)


-- | 6. Create idea
data PageCreateIdea = PageCreateIdea
  deriving (Eq, Show, Read)

instance ToMarkup PageCreateIdea where
    toMarkup _ = "PageCreateIdea"


-- | 7. Edit idea
data PageEditIdea = PageEditIdea
  deriving (Eq, Show, Read)

instance ToMarkup PageEditIdea where
    toMarkup _ = "PageEditIdea"


-- | 8. User profile
data PageUserProfile (a :: UserProfilePageState) = PageUserProfile
  deriving (Eq, Show, Read)

data UserProfilePageState =
    UserProfileCreateIdeas     -- ^ 8.1 User profile: Created ideas
  | UserProfileDelegatedVotes  -- ^ 8.2 User profile: Delegated votes
  deriving (Eq, Show, Enum, Bounded, Read)

instance ToMarkup (PageUserProfile a) where
    toMarkup _ = "PageUserProfile UserProfilePageState"


-- | 9. User settings
data PageUserSettings = PageUserSettings
  deriving (Eq, Show, Read)

instance ToMarkup PageUserSettings where
    toMarkup _ = "PageUserSettings"


-- | 10. Create topic
data PageCreateTopic (a :: CreateTopicPageState) = PageCreateTopic
  deriving (Eq, Show, Read)

data CreateTopicPageState =
    CreateTopicS1  -- ^ 10.1 Create topic: Create topic
  | CreateTopicS2  -- ^ 10.2 Create topic: Move ideas to topic
  deriving (Eq, Show, Enum, Bounded, Read)

instance ToMarkup (PageCreateTopic a) where
    toMarkup _ = "PageCreateTopic CreateTopicPageState"


-- | 11.1 Admin settings: Durations & quorum
data PageAdminSettingsDurationsAndQuorum =
    PageAdminSettingsDurationsAndQuorum
  deriving (Eq, Show, Read)

instance ToMarkup PageAdminSettingsDurationsAndQuorum where
    toMarkup _ = "PageAdminSettingsDurationsAndQuorum"


-- | 11.2 Admin settings: Manage groups & permissions
data PageAdminSettingsGroupsAndPermissions =
    PageAdminSettingsGroupsAndPermissions
  deriving (Eq, Show, Read)

instance ToMarkup PageAdminSettingsGroupsAndPermissions where
    toMarkup _ = "PageAdminSettingsGroupsAndPermissions"


-- | 11.3 Admin settings: User creation & user import
data PageAdminSettingsUserCreateAndImport =
    PageAdminSettingsUserCreateAndImport
  deriving (Eq, Show, Read)

instance ToMarkup PageAdminSettingsUserCreateAndImport where
    toMarkup _ = "PageAdminSettingsUserCreateAndImport"


-- | 11.4 Admin settings: Events protocol
data PageAdminSettingsEventsProtocol =
    PageAdminSettingsEventsProtocol
  deriving (Eq, Show, Read)

instance ToMarkup PageAdminSettingsEventsProtocol where
    toMarkup _ = "PageAdminSettingsEventsProtocol"


-- | 12. Delegate vote
data PageDelegateVote = PageDelegateVote
  deriving (Eq, Show, Read)

instance ToMarkup PageDelegateVote where
    toMarkup _ = "PageDelegateVote"


-- | 13. Delegation network
data PageDelegationNetwork = PageDelegationNetwork
  deriving (Eq, Show, Read)

instance ToMarkup PageDelegationNetwork where
    toMarkup _ = "PageDelegationNetwork"


-- | 14. Static page: Imprint
data PageStaticImprint = PageStaticImprint
  deriving (Eq, Show, Read)

instance ToMarkup PageStaticImprint where
    toMarkup _ = "PageStaticImprint"


-- | 15. Static page: Terms of use
data PageStaticTermsOfUse = PageStaticTermsOfUse
  deriving (Eq, Show, Read)

instance ToMarkup PageStaticTermsOfUse where
    toMarkup _ = "PageStaticTermsOfUse"


-- | 16. Home page with login prompt
data PageHomeWithLoginPrompt = PageHomeWithLoginPrompt
  deriving (Eq, Show, Read)

instance ToMarkup PageHomeWithLoginPrompt where
    toMarkup _ = "PageHomeWithLoginPrompt"





data PageIdea = PageIdea Idea
  deriving (Eq, Show, Read)

instance ToMarkup PageIdea where
    toMarkup (PageIdea idea) = div $ do
        h2 . text $ idea ^. ideaTitle

        div . toMarkup . AuthorWidget $ idea ^. ideaMeta
        div . string . show $ idea ^. ideaCategory

        -- von X / X stimmen / X verbesserungvorschläge
        div $ do
            span . text $ "von " <> (cs . show $ idea ^. ideaMeta . metaCreatedBy )
            span . text $ "/"
            span . string $ (show . Set.size $ idea ^. ideaVotes) <> " Stimmen"
            span . text $ "/"
            span . string $ (show . Set.size $ idea ^. ideaComments) <> " Verbesserungsvorschläge"

        -- visual vote stats
        div . pre $ do
            let y = countVotes Yes ideaVoteValue $ idea ^. ideaVotes
                n = countVotes No  ideaVoteValue $ idea ^. ideaVotes
            div $ do
                span . string $ "    " <> replicate y '+' <> ":" <> replicate n '-'
            div $ do
                span . string $ replicate (4 + y - length (show y)) ' ' <> show y <> ":" <> show n

        -- buttons
        div $ do
            button ! value "yes"     $ text "dafür"
            button ! value "neutral" $ text "neutral"
            button ! value "no"      $ text "dagegen"

        -- article
        div . toMarkup $ idea ^. ideaDesc

        -- comments
        div $ do
            hr
            span . string $ (show . Set.size $ idea ^. ideaComments) <> " Verbesserungsvorschläge"
            span $ button ! value "create_comment" $ text "Neuer Verbesserungsvorschlag"
            hr
            sequence_ . (toMarkup . PageComment <$>) . Set.toList $ idea ^. ideaComments

data PageComment = PageComment Comment
  deriving (Eq, Show, Read)

instance ToMarkup PageComment where
    toMarkup (PageComment comment) = div $ do
        div $ do
            span . toMarkup . AuthorWidget $ comment ^. commentMeta
            span . toMarkup . VotesWidget  $ comment ^. commentVotes
        div $ do
            toMarkup $ comment ^. commentText
        div $ do
            span $ text "[antworten]"
            span $ text "[melden]"

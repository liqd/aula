{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
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

import Control.Lens ((^.))
import Control.Monad (forM_)
import Data.Set (Set)
import Data.String.Conversions
import Data.Typeable (Typeable)
import Lucid
import Lucid.Base
import Prelude hiding (head, span, div)

import qualified Data.Set as Set

import Api
import Types
import Frontend.Core


----------------------------------------------------------------------
-- building blocks


newtype CommentVotesWidget = VotesWidget (Set CommentVote)

instance ToHtml CommentVotesWidget where
    toHtmlRaw = toHtml
    toHtml p@(VotesWidget votes) = semanticDiv p . toHtml $ y ++ n
      where
        y = "[up: "   <> show (countVotes Up   commentVoteValue votes) <> "]"
        n = "[down: " <> show (countVotes Down commentVoteValue votes) <> "]"

newtype AuthorWidget a = AuthorWidget (MetaInfo a)

instance (Typeable a) => ToHtml (AuthorWidget a) where
    toHtmlRaw = toHtml
    toHtml p@(AuthorWidget mi) = semanticDiv p . span_ $ do
        "["
        img_ [src_ $ mi ^. metaCreatedByAvatar]
        toHtml $ mi ^. metaCreatedByLogin
        "]"



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
        p_ $ "WILDE IDEEN"
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
        div_ $ mapM_ (toHtml . ListItemIdea) ideas


-- | 3. Ideas in discussion
data PageIdeasInDiscussion = PageIdeasInDiscussion
  deriving (Eq, Show, Read)

instance ToHtml PageIdeasInDiscussion where
    toHtmlRaw = toHtml
    toHtml p = semanticDiv p "PageIdeasInDiscussion"


-- | 4.1 Topic overview: Refinement phase
data PageTopicOverviewRefinementPhase = PageTopicOverviewRefinementPhase
  deriving (Eq, Show, Read)

instance ToHtml PageTopicOverviewRefinementPhase where
    toHtmlRaw = toHtml
    toHtml p = semanticDiv p "PageTopicOverviewRefinementPhase"


-- | 4.2 Topic overview: Assessment phase
data PageTopicOverviewAssessmentPhase = PageTopicOverviewAssessmentPhase
  deriving (Eq, Show, Read)

instance ToHtml PageTopicOverviewAssessmentPhase where
    toHtmlRaw = toHtml
    toHtml p = semanticDiv p "PageTopicOverviewAssessmentPhase"


-- | 4.3 Topic overview: Voting phase
data PageTopicOverviewVotingPhase = PageTopicOverviewVotingPhase
  deriving (Eq, Show, Read)

instance ToHtml PageTopicOverviewVotingPhase where
    toHtmlRaw = toHtml
    toHtml p = semanticDiv p "PageTopicOverviewVotingPhase"


-- | 4.4 Topic overview: Result phase
data PageTopicOverviewResultPhase = PageTopicOverviewResultPhase
  deriving (Eq, Show, Read)

instance ToHtml PageTopicOverviewResultPhase where
    toHtmlRaw = toHtml
    toHtml p = semanticDiv p "PageTopicOverviewResultPhase"


-- | 4.5 Topic overview: Delegations
data PageTopicOverviewDelegations = PageTopicOverviewDelegations
  deriving (Eq, Show, Read)

instance ToHtml PageTopicOverviewDelegations where
    toHtmlRaw = toHtml
    toHtml p = semanticDiv p "PageTopicOverviewDelegations"


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


-- | 5.3 Idea detail page: Assessment phase
data PageIdeaDetailAssessmentPhase = PageIdeaDetailAssessmentPhase
    deriving (Eq, Show, Read)

instance ToHtml PageIdeaDetailAssessmentPhase where
    toHtmlRaw = toHtml
    toHtml p = semanticDiv p "PageIdeaDetailAssessmentPhase"


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


-- | 10.1 Create topic: Create topic
data PageCreateTopic = PageCreateTopic
  deriving (Eq, Show, Read)

instance ToHtml PageCreateTopic where
    toHtmlRaw = toHtml
    toHtml p = semanticDiv p "PageCreateTopic"


-- | 10.2 Create topic: Move ideas to topic
data PageCreateTopicAddIdeas = PageCreateTopicAddIdeas
  deriving (Eq, Show, Read)

instance ToHtml PageCreateTopicAddIdeas where
    toHtmlRaw = toHtml
    toHtml p = semanticDiv p "PageCreateTopicAddIdeas"


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


-- | 14. Static page: Imprint
data PageStaticImprint = PageStaticImprint
  deriving (Eq, Show, Read)

instance ToHtml PageStaticImprint where
    toHtmlRaw = toHtml
    toHtml p = semanticDiv p "PageStaticImprint"


-- | 15. Static page: Terms of use
data PageStaticTermsOfUse = PageStaticTermsOfUse
  deriving (Eq, Show, Read)

instance ToHtml PageStaticTermsOfUse where
    toHtmlRaw = toHtml
    toHtml p = semanticDiv p "PageStaticTermsOfUse"


-- | 16. Home page with login prompt
data PageHomeWithLoginPrompt = PageHomeWithLoginPrompt
  deriving (Eq, Show, Read)

instance ToHtml PageHomeWithLoginPrompt where
    toHtmlRaw = toHtml
    toHtml p = semanticDiv p "PageHomeWithLoginPrompt"





data ListItemIdea = ListItemIdea Idea
  deriving (Eq, Show, Read)

instance ToHtml ListItemIdea where
    toHtmlRaw = toHtml
    toHtml p@(ListItemIdea idea) = semanticDiv p $ do
        span_ $ do
            img_ [src_ "some_avatar"]
        span_ $ do
            div_ . toHtml $ idea ^. ideaTitle
            div_ . toHtml $ "von " <> idea ^. (ideaMeta . metaCreatedByLogin)
        span_ $ do
            span_ $ do
                let s = Set.size (idea ^. ideaComments)
                toHtml $ show s
                if s == 1 then "Verbesserungsvorschlag" else "Verbesserungsvorschlaege"
            -- TODO: show how many votes are in and how many are required


data PageIdea = PageIdea Idea
  deriving (Eq, Show, Read)

instance ToHtml PageIdea where
    toHtmlRaw = toHtml
    toHtml p@(PageIdea idea) = semanticDiv p $ do
        h2_ . toHtml $ idea ^. ideaTitle

        div_ . toHtml . AuthorWidget $ idea ^. ideaMeta
        div_ . toHtml . show         $ idea ^. ideaCategory

        -- von X / X stimmen / X verbesserungvorschläge
        div_ $ do
            span_ $ "von " <> (toHtml . show $ idea ^. ideaMeta . metaCreatedBy)
            span_ $ "/"
            span_ $ (toHtml . show . Set.size $ idea ^. ideaVotes) <> " Stimmen"
            span_ $ "/"
            span_ $ (toHtml . show . Set.size $ idea ^. ideaComments) <> " Verbesserungsvorschläge"

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
            button_ [makeAttribute "yes"     "dafür"]   (pure ())  -- FIXME: is there a nicer way to say this?
            button_ [makeAttribute "neutral" "neutral"] (pure ())
            button_ [makeAttribute "no"      "dagegen"] (pure ())

        -- article
        div_ . toHtml $ idea ^. ideaDesc

        -- comments
        div_ $ do
            hr_ []
            span_ $ (toHtml . show . Set.size $ idea ^. ideaComments) <> " Verbesserungsvorschläge"
            span_ $ button_ [makeAttribute "create_comment" "Neuer Verbesserungsvorschlag"] (pure ())
            hr_ []
            sequence_ . (toHtml . PageComment <$>) . Set.toList $ idea ^. ideaComments

data PageComment = PageComment Comment
  deriving (Eq, Show, Read)

instance ToHtml PageComment where
    toHtmlRaw = toHtml
    toHtml p@(PageComment comment) = semanticDiv p $ do
        div_ $ do
            span_ . toHtml . AuthorWidget $ comment ^. commentMeta
            span_ . toHtml . VotesWidget  $ comment ^. commentVotes
        div_ $ do
            toHtml $ comment ^. commentText
        div_ $ do
            span_ "[antworten]"
            span_ "[melden]"

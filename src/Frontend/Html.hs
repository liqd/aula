{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE OverloadedStrings  #-}

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
import Control.Monad (forM_)
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
        body (headerMarkup >> toMarkup bdy >> footerMarkup)

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

headerMarkup :: Html
headerMarkup = div $ do
    span $ text "aula"
    -- TODO: these should be links
    span $ text "Ideenräume"
    span $ text "Beauftragungsnetzwerk"
    span $ text "Hi VorNac"
    span $ img ! src "the_avatar"
    hr

footerMarkup :: Html
footerMarkup = div $ do
    hr
    -- TODO: these should be links
    span $ text "Nutzungsbedingungen"
    span $ text "Impressum"
    -- Should be on the right (and we need to specify encoding in html)
    span $ text "Made with ♡ by Liqd"


----------------------------------------------------------------------
-- pages

-- | 1. Rooms overview
data PageRoomsOverview = PageRoomsOverview [String]
  deriving (Eq, Show, Read)

instance ToMarkup PageRoomsOverview where
    toMarkup (PageRoomsOverview rooms) = case rooms of
      [] -> p "Keine Ideenräume"
      _  -> forM_ rooms $ div . toMarkup


-- | 2. Ideas overview
data PageIdeasOverview = PageIdeasOverview [Idea]
  deriving (Eq, Show, Read)

instance ToMarkup PageIdeasOverview where
    toMarkup (PageIdeasOverview ideas) = do
        p $ "WILDE IDEEN"
        h1 "Was soll sich verändern?"
        p $ "Du kannst hier jede lose Idee, die du im Kopf hast, einwerfen und kannst fuer die "
            <> "Idee abstimmen und diese somit \"auf den Tisch bringen\"."
        div $ button $ text "+ Neue Idee" -- FIXME: should link to idea creation form
        div $ do
            -- FIXME: these buttons should filter the ideas by category
            button $ text "Regeln"
            button $ text "Ausstattung"
            button $ text "Unterricht"
            button $ text "Zeit"
            button $ text "Umgebung"
        div $ mapM_ (toMarkup . ListItemIdea) ideas


-- | 3. Ideas in discussion
data PageIdeasInDiscussion = PageIdeasInDiscussion
  deriving (Eq, Show, Read)

instance ToMarkup PageIdeasInDiscussion where
    toMarkup _ = "PageIdeasInDiscussion"


-- | 4.1 Topic overview: Refinement phase
data PageTopicOverviewRefinementPhase = PageTopicOverviewRefinementPhase
  deriving (Eq, Show, Read)

instance ToMarkup PageTopicOverviewRefinementPhase where
    toMarkup _ = "PageTopicOverviewRefinementPhase"


-- | 4.2 Topic overview: Assessment phase
data PageTopicOverviewAssessmentPhase = PageTopicOverviewAssessmentPhase
  deriving (Eq, Show, Read)

instance ToMarkup PageTopicOverviewAssessmentPhase where
    toMarkup _ = "PageTopicOverviewAssessmentPhase"


-- | 4.3 Topic overview: Voting phase
data PageTopicOverviewVotingPhase = PageTopicOverviewVotingPhase
  deriving (Eq, Show, Read)

instance ToMarkup PageTopicOverviewVotingPhase where
    toMarkup _ = "PageTopicOverviewVotingPhase"


-- | 4.4 Topic overview: Result phase
data PageTopicOverviewResultPhase = PageTopicOverviewResultPhase
  deriving (Eq, Show, Read)

instance ToMarkup PageTopicOverviewResultPhase where
    toMarkup _ = "PageTopicOverviewResultPhase"


-- | 4.5 Topic overview: Delegations
data PageTopicOverviewDelegations = PageTopicOverviewDelegations
  deriving (Eq, Show, Read)

instance ToMarkup PageTopicOverviewDelegations where
    toMarkup _ = "PageTopicOverviewDelegations"


-- | 5.1 Idea detail page: New ideas
data PageIdeaDetailNewIdeas = PageIdeaDetailNewIdeas Idea
    deriving (Eq, Show, Read)

instance ToMarkup PageIdeaDetailNewIdeas where
    toMarkup (PageIdeaDetailNewIdeas idea) = toMarkup (PageIdea idea)


-- | 5.2 Idea detail page: Refinement phase
data PageIdeaDetailRefinementPhase = PageIdeaDetailRefinementPhase
    deriving (Eq, Show, Read)

instance ToMarkup PageIdeaDetailRefinementPhase where
    toMarkup _ = "PageIdeaDetailRefinementPhase"


-- | 5.3 Idea detail page: Assessment phase
data PageIdeaDetailAssessmentPhase = PageIdeaDetailAssessmentPhase
    deriving (Eq, Show, Read)

instance ToMarkup PageIdeaDetailAssessmentPhase where
    toMarkup _ = "PageIdeaDetailAssessmentPhase"


-- | 5.4 Idea detail page: Voting phase
data PageIdeaDetailVotingPhase = PageIdeaDetailVotingPhase
    deriving (Eq, Show, Read)

instance ToMarkup PageIdeaDetailVotingPhase where
    toMarkup _ = "PageIdeaDetailVotingPhase"


-- | 5.5 Idea detail page: Move idea to topic
data PageIdeaDetailMoveIdeaToTopic = PageIdeaDetailMoveIdeaToTopic
    deriving (Eq, Show, Read)

instance ToMarkup PageIdeaDetailMoveIdeaToTopic where
    toMarkup _ = "PageIdeaDetailMoveIdeaToTopic"


-- | 5.6 Idea detail page: Feasible / not feasible
data PageIdeaDetailFeasibleNotFeasible = PageIdeaDetailFeasibleNotFeasible
    deriving (Eq, Show, Read)

instance ToMarkup PageIdeaDetailFeasibleNotFeasible where
    toMarkup _ = "PageIdeaDetailFeasibleNotFeasible"


-- | 5.7 Idea detail page: Winner
data PageIdeaDetailWinner = PageIdeaDetailWinner
    deriving (Eq, Show, Read)

instance ToMarkup PageIdeaDetailWinner where
    toMarkup _ = "PageIdeaDetailWinner"


-- | 6. Create idea
data PageCreateIdea = PageCreateIdea ST
  deriving (Eq, Show, Read)

instance ToMarkup PageCreateIdea where
    toMarkup (PageCreateIdea t) = do
        p . text $ "added: " <> t

-- | 7. Edit idea
data PageEditIdea = PageEditIdea
  deriving (Eq, Show, Read)

instance ToMarkup PageEditIdea where
    toMarkup _ = "PageEditIdea"


-- | 8.1 User profile: Created ideas
data PageUserProfileCreateIdeas = PageUserProfileCreateIdeas
  deriving (Eq, Show, Read)

instance ToMarkup PageUserProfileCreateIdeas where
    toMarkup _ = "PageUserProfileCreateIdeas"


-- | 8.2 User profile: Delegated votes
data PageUserProfileDelegatedVotes = PageUserProfileDelegatedVotes
  deriving (Eq, Show, Read)

instance ToMarkup PageUserProfileDelegatedVotes where
    toMarkup _ = "PageUserProfileDelegatedVotes"


-- | 9. User settings
data PageUserSettings = PageUserSettings
  deriving (Eq, Show, Read)

instance ToMarkup PageUserSettings where
    toMarkup _ = "PageUserSettings"


-- | 10.1 Create topic: Create topic
data PageCreateTopic = PageCreateTopic
  deriving (Eq, Show, Read)

instance ToMarkup PageCreateTopic where
    toMarkup _ = "PageCreateTopic"


-- | 10.2 Create topic: Move ideas to topic
data PageCreateTopicAddIdeas = PageCreateTopicAddIdeas
  deriving (Eq, Show, Read)

instance ToMarkup PageCreateTopicAddIdeas where
    toMarkup _ = "PageCreateTopicAddIdeas"


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





data ListItemIdea = ListItemIdea Idea
  deriving (Eq, Show, Read)

instance ToMarkup ListItemIdea where
    toMarkup (ListItemIdea idea) = div $ do
        span $ do
            img ! src "some_avatar"
        span $ do
            div (text $ idea ^. ideaTitle)
            div (text "von " >> (renderUserName $ idea ^.(ideaMeta . metaCreatedByLogin)))
        span $ do
            span $ do
                let s = Set.size (idea ^. ideaComments)
                toMarkup s
                text $ if s == 1 then "Verbesserungsvorschlag" else "Verbesserungsvorschlaege"
            -- TODO: show how many votes are in and how many are required
      where
        renderUserName n = text n  -- TODO: link to user profile


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

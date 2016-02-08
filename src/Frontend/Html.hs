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

headerMarkup :: Markup
headerMarkup = aulaLogo >> userAvatar >> hr
  where
    aulaLogo = span $ text "aula"
    userAvatar = span $ text "MyUserName" -- should be on the right

footerMarkup :: Markup
footerMarkup = span $ do
    hr
    -- TODO: these should be links
    text "Nutzungsbedingungen"
    text "Impressum"
    -- Should be on the right (and we need to specify encoding in html)
    text "Made with ♡ by Liqd"


----------------------------------------------------------------------
-- pages

-- | 1. Rooms overview
data PageRoomsOverview = PageRoomsOverview [String] -- should be a list of rooms,
    -- but the click dummy doesn't show any rooms
  deriving (Eq, Show, Read)

instance ToMarkup PageRoomsOverview where
    toMarkup (PageRoomsOverview rooms) = ul $
        forM_ rooms $ li . toMarkup


-- | 2. Ideas overview
data PageIdeasOverview = PageIdeasOverview [Idea]
  deriving (Eq, Show, Read)

instance ToMarkup PageIdeasOverview where
    toMarkup (PageIdeasOverview ideas) = do
        -- "WILDE IDEEN"
        h1 "Was soll sich veraendern?"
        p $ "Du kannst hier jede lose Idee, die du im Kopf hast, einwerfen und kannst fuer die Idee abstimmen und diese somit >>auf den Tisch bringen <<"
        p $ button $ text "+ Neue Idee" -- FIXME: should link to idea creation form
        p $ do
            -- FIXME: these buttons should filter the ideas by category
            button $ text "Regeln"
            button $ text "Ausstattung"
            button $ text "Unterricht"
            button $ text "Zeit"
            button $ text "Umgebung"
        ul $ mapM_ renderIdeaEntry ideas
      where
        renderIdeaEntry :: Idea -> Markup
        renderIdeaEntry idea = li . div $ do
            span $ do
                h2 (text $ idea ^. ideaTitle) >> br
                text "von " >> (renderUserName $ idea ^.(ideaMeta . metaCreatedByLogin))
            span $ renderRightColumn idea
        renderUserName name = text name -- TODO: link to user profile
        renderRightColumn idea = do
            p $ do
                toMarkup (Set.size (idea ^. ideaComments))
                text " Verbesserungsvorschlaege"
            -- FIXME: show how many votes are in and how many are required


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


-- | 11. Admin settings
data PageAdminSettings (a :: AdminSettingsPageState) = PageAdminSettings
  deriving (Eq, Show, Read)

data AdminSettingsPageState =
    AdminSettingsDurationsAndQuorum          -- ^ 11.1 Admin settings: Durations & quorum
  | AdminSettingsManageGroupsAndPermissions  -- ^ 11.2 Admin settings: Manage groups & permissions
  | AdminSettingsUserCreateAndImport         -- ^ 11.3 Admin settings: User creation & user import
  | AdminSettingsEventsProtocol              -- ^ 11.4 Admin settings: Events protocol
  deriving (Eq, Show, Enum, Bounded, Read)

instance ToMarkup (PageAdminSettings a) where
    toMarkup _ = "PageAdminSettings AdminSettingsPageState"


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

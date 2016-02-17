{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ImpredicativeTypes  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

{-# OPTIONS_GHC -fno-warn-orphans -Werror #-}

module Arbitrary (topLevelDomains, loremIpsum, generate, arbitrary) where

import Control.Monad (replicateM)
import Data.Char
import Data.List as List
import Data.String.Conversions (ST, cs, (<>))
import Data.Text as ST
import Test.QuickCheck (Arbitrary(..), Gen, elements, oneof, scale, generate, arbitrary)
import Test.QuickCheck.Instances ()

import Types
import Frontend.Page


----------------------------------------------------------------------
-- pages

instance Arbitrary PageRoomsOverview where
    arbitrary = PageRoomsOverview <$> arbitrary

instance Arbitrary PageIdeasOverview where
    arbitrary = PageIdeasOverview <$> arb

instance Arbitrary PageIdeasInDiscussion where
    arbitrary = pure PageIdeasInDiscussion

instance Arbitrary PageTopicOverviewRefinementPhase where
    arbitrary = PageTopicOverviewRefinementPhase <$> arb <*> arb

instance Arbitrary PageTopicOverviewJuryPhase where
    arbitrary = PageTopicOverviewJuryPhase <$> arb <*> arb

instance Arbitrary PageTopicOverviewVotingPhase where
    arbitrary = PageTopicOverviewVotingPhase <$> arb <*> arb

instance Arbitrary PageTopicOverviewResultPhase where
    arbitrary = PageTopicOverviewResultPhase <$> arb <*> arb

instance Arbitrary PageTopicOverviewDelegations where
    arbitrary = pure PageTopicOverviewDelegations

instance Arbitrary PageIdeaDetailNewIdeas where
    arbitrary = PageIdeaDetailNewIdeas <$> arb

instance Arbitrary PageIdeaDetailRefinementPhase where
    arbitrary = pure PageIdeaDetailRefinementPhase

instance Arbitrary PageIdeaDetailJuryPhase where
    arbitrary = pure PageIdeaDetailJuryPhase

instance Arbitrary PageIdeaDetailVotingPhase where
    arbitrary = pure PageIdeaDetailVotingPhase

instance Arbitrary PageIdeaDetailMoveIdeaToTopic where
    arbitrary = pure PageIdeaDetailMoveIdeaToTopic

instance Arbitrary PageIdeaDetailFeasibleNotFeasible where
    arbitrary = pure PageIdeaDetailFeasibleNotFeasible

instance Arbitrary PageIdeaDetailWinner where
    arbitrary = pure PageIdeaDetailWinner

instance Arbitrary PageCreateIdea where
    arbitrary = pure PageCreateIdea

instance Arbitrary PageEditIdea where
    arbitrary = pure PageEditIdea

instance Arbitrary PageUserProfileCreateIdeas where
    arbitrary = pure PageUserProfileCreateIdeas

instance Arbitrary PageUserProfileDelegatedVotes where
    arbitrary = pure PageUserProfileDelegatedVotes

instance Arbitrary PageUserSettings where
    arbitrary = pure PageUserSettings

instance Arbitrary PageCreateTopic where
    arbitrary = pure PageCreateTopic

instance Arbitrary PageCreateTopicAddIdeas where
    arbitrary = pure PageCreateTopicAddIdeas

instance Arbitrary PageAdminSettingsDurationsAndQuorum where
    arbitrary = pure PageAdminSettingsDurationsAndQuorum

instance Arbitrary PageAdminSettingsGroupsAndPermissions where
    arbitrary = pure PageAdminSettingsGroupsAndPermissions

instance Arbitrary PageAdminSettingsUserCreateAndImport where
    arbitrary = pure PageAdminSettingsUserCreateAndImport

instance Arbitrary PageAdminSettingsEventsProtocol where
    arbitrary = pure PageAdminSettingsEventsProtocol

instance Arbitrary PageDelegateVote where
    arbitrary = pure PageDelegateVote

instance Arbitrary PageDelegationNetwork where
    arbitrary = pure PageDelegationNetwork

instance Arbitrary PageStaticImprint where
    arbitrary = pure PageStaticImprint

instance Arbitrary PageStaticTermsOfUse where
    arbitrary = pure PageStaticTermsOfUse

instance Arbitrary PageHomeWithLoginPrompt where
    arbitrary = pure PageHomeWithLoginPrompt

instance Arbitrary LoginFormData where
    arbitrary = LoginFormData <$> arbWord <*> arbWord


----------------------------------------------------------------------
-- idea

instance Arbitrary ProtoIdea where
    arbitrary = ProtoIdea <$> arbWord <*> arb <*> arb

instance Arbitrary Idea where
    arbitrary = Idea <$> arb <*> arbPhrase <*> arb'
                     <*> arb' <*> arb' <*> arb'
                     <*> arb' <*> arb' <*> arb'
                     <*> arb' <*> arb'

instance Arbitrary Category where
    arbitrary = elements [minBound..]

instance Arbitrary IdeaLike where
    arbitrary = IdeaLike <$> arb

instance Arbitrary IdeaVote where
    arbitrary = IdeaVote <$> arb <*> arb

instance Arbitrary IdeaVoteValue where
    arbitrary = elements [minBound..]

instance Arbitrary Feasible where
    arbitrary = Feasible <$> arb <*> arb <*> arb


----------------------------------------------------------------------
-- comment

instance Arbitrary Comment where
    arbitrary = Comment <$> arb <*> arb <*> arb' <*> arb'

instance Arbitrary CommentVote where
    arbitrary = CommentVote <$> arb <*> arb

instance Arbitrary UpDown where
    arbitrary = elements [minBound..]


----------------------------------------------------------------------
-- idea space, topic, phase

instance Arbitrary IdeaSpace where
    arbitrary = oneof [pure SchoolSpace, ClassSpace <$> arb]

instance Arbitrary SchoolClass where
    arbitrary = SchoolClass <$> name <*> year
      where
        name = elements [ cs $ show age <> [branch] | age <- [1..12 :: Int], branch <- ['a'..'e'] ]
        year = elements $ cs . show <$> [2012..2020 :: Int]

instance Arbitrary ProtoTopic where
    arbitrary = ProtoTopic <$> arbPhrase <*> arb' <*> arb' <*> arb' <*> arb'

instance Arbitrary Topic where
    arbitrary = Topic <$> arb <*> arbPhrase <*> arb' <*> arb' <*> arb' <*> arb'

instance Arbitrary Phase where
    arbitrary = elements [minBound..]


----------------------------------------------------------------------
-- user

instance Arbitrary User where
    arbitrary = User <$> arb <*> arbWord <*> arbWord <*> arb <*> arb <*> arb <*> arb <*> arb

instance Arbitrary Group where
    arbitrary = oneof
        [ Student <$> arb
        , ClassGuest <$> arb
        , pure SchoolGuest
        , pure Moderator
        , pure Principal
        , pure Admin
        ]

instance Arbitrary EncryptedPass where
    arbitrary = EncryptedPass <$> arb

instance Arbitrary Email where
    arbitrary = do
        localName  <- arbWord
        domainName <- arbWord
        tld        <- elements topLevelDomains
        return . Email . mconcat $ [localName, "@", domainName, ".", tld]

-- FIXME: instance Arbitrary Delegation

-- FIXME: instance Arbitrary DelegationContext


----------------------------------------------------------------------
-- aula-specific helpers

instance Arbitrary (AUID a) where
    arbitrary = AUID <$> arb

instance Arbitrary (MetaInfo a) where
    arbitrary = MetaInfo <$> arb <*> arb <*> arbWord <*> arbPhrase <*> arb <*> arb <*> arb

instance Arbitrary Document where
    arbitrary = Markdown . ST.unlines . fmap fromParagraph <$> scale (`div` 5) arb

instance (Arbitrary a) => Arbitrary (PageShow a) where
    arbitrary = PageShow <$> arb

----------------------------------------------------------------------
-- general-purpose helpers

arb :: Arbitrary a => Gen a
arb = arbitrary

arb' :: Arbitrary a => Gen a
arb' = scale (`div` 3) arb

instance Arbitrary Timestamp where
    arbitrary = Timestamp <$> arb


----------------------------------------------------------------------
-- arbitrary readable text

-- | source: lipsum.com
loremIpsum :: [ST]
loremIpsum = ST.unlines <$>
  [ -- The standard Lorem Ipsum passage, used since the 1500s
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do" :
    "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad" :
    "minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip" :
    "ex ea commodo consequat. Duis aute irure dolor in reprehenderit in" :
    "voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur" :
    "sint occaecat cupidatat non proident, sunt in culpa qui officia" :
    "deserunt mollit anim id est laborum." :
    []

  , -- Section 1.10.32 of "de Finibus Bonorum et Malorum", written by Cicero in 45 BC
    "Sed ut perspiciatis unde omnis iste natus error sit voluptatem" :
    "accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae" :
    "ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt" :
    "explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut" :
    "odit aut fugit, sed quia consequuntur magni dolores eos qui ratione" :
    "voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum" :
    "quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam" :
    "eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat" :
    "voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam" :
    "corporis suscipit laboriosam, nisi ut aliquid ex ea commodi" :
    "consequatur? Quis autem vel eum iure reprehenderit qui in ea voluptate" :
    "velit esse quam nihil molestiae consequatur, vel illum qui dolorem eum" :
    "fugiat quo voluptas nulla pariatur?" :
    []

  , -- 1914 translation by H. Rackham
    "But I must explain to you how all this mistaken idea of denouncing" :
    "pleasure and praising pain was born and I will give you a complete" :
    "account of the system, and expound the actual teachings of the great" :
    "explorer of the truth, the master-builder of human happiness. No one" :
    "rejects, dislikes, or avoids pleasure itself, because it is pleasure," :
    "but because those who do not know how to pursue pleasure rationally" :
    "encounter consequences that are extremely painful. Nor again is there" :
    "anyone who loves or pursues or desires to obtain pain of itself," :
    "because it is pain, but because occasionally circumstances occur in" :
    "which toil and pain can procure him some great pleasure. To take a" :
    "trivial example, which of us ever undertakes laborious physical" :
    "exercise, except to obtain some advantage from it? But who has any" :
    "right to find fault with a man who chooses to enjoy a pleasure that has" :
    "no annoying consequences, or one who avoids a pain that produces no" :
    "resultant pleasure?" :
    []

  , -- Section 1.10.33 of "de Finibus Bonorum et Malorum", written by Cicero in 45 BC
    "At vero eos et accusamus et iusto odio dignissimos ducimus qui" :
    "blanditiis praesentium voluptatum deleniti atque corrupti quos dolores" :
    "et quas molestias excepturi sint occaecati cupiditate non provident," :
    "similique sunt in culpa qui officia deserunt mollitia animi, id est" :
    "laborum et dolorum fuga. Et harum quidem rerum facilis est et expedita" :
    "distinctio. Nam libero tempore, cum soluta nobis est eligendi optio" :
    "cumque nihil impedit quo minus id quod maxime placeat facere possimus," :
    "omnis voluptas assumenda est, omnis dolor repellendus. Temporibus autem" :
    "quibusdam et aut officiis debitis aut rerum necessitatibus saepe" :
    "eveniet ut et voluptates repudiandae sint et molestiae non recusandae." :
    "Itaque earum rerum hic tenetur a sapiente delectus, ut aut reiciendis" :
    "voluptatibus maiores alias consequatur aut perferendis doloribus" :
    "asperiores repellat." :
    []

  , -- 1914 translation by H. Rackham
    "On the other hand, we denounce with righteous indignation and dislike" :
    "men who are so beguiled and demoralized by the charms of pleasure of" :
    "the moment, so blinded by desire, that they cannot foresee the pain and" :
    "trouble that are bound to ensue; and equal blame belongs to those who" :
    "fail in their duty through weakness of will, which is the same as" :
    "saying through shrinking from toil and pain. These cases are perfectly" :
    "simple and easy to distinguish. In a free hour, when our power of" :
    "choice is untrammelled and when nothing prevents our being able to do" :
    "what we like best, every pleasure is to be welcomed and every pain" :
    "avoided. But in certain circumstances and owing to the claims of duty" :
    "or the obligations of business it will frequently occur that pleasures" :
    "have to be repudiated and annoyances accepted. The wise man therefore" :
    "always holds in these matters to this principle of selection: he" :
    "rejects pleasures to secure other greater pleasures, or else he endures" :
    "pains to avoid worse pains." :
    []

  , -- 25 most common adjectives according to the Oxford English Dictionary.
    "good" : "new" : "first" : "last" : "long" : "great" : "little" :
    "own" : "other" : "old" : "right" : "big" : "high" : "different" :
    "small" : "large" : "next" : "early" : "young" : "important" :
    "few" : "public" : "bad" : "same" : "able" :
    []
  ]

loremIpsumDict :: [ST]
loremIpsumDict = nub . sort . mconcat $ ST.words <$> loremIpsum

arbWord :: Gen ST
arbWord = ST.filter isAlpha <$> elements loremIpsumDict

arbPhrase :: Gen ST
arbPhrase = do
    n <- (+ 3) . (`mod` 5) <$> arbitrary
    ST.intercalate " " <$> replicateM n arbWord

newtype Paragraph = Paragraph { fromParagraph :: ST }

instance Arbitrary Paragraph where
    arbitrary = Paragraph <$> (arbitrary >>= create . (+ 13) . abs)
      where
        create :: Int -> Gen ST
        create n = terminate <$> replicateM n (elements loremIpsumDict)

        terminate :: [ST] -> ST
        terminate (ST.unwords -> xs) = (if isAlpha $ ST.last xs then ST.init xs else xs) <> "."


topLevelDomains :: [ST]
topLevelDomains = ["com", "net", "org", "info", "de", "fr", "ru", "co.uk"]

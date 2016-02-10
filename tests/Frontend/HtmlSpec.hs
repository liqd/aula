{-# LANGUAGE GADTs #-}
module Frontend.HtmlSpec where

import Arbitrary
import Frontend.Html

import Control.Applicative ((<$>))
import Data.Typeable (Typeable, typeOf)
import Text.Blaze (ToMarkup(..))
import Text.Blaze.Html.Renderer.String (renderHtml)

import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Arbitrary(..), Gen, Testable, forAll, property)

spec :: Spec
spec = do
    mapM_ renderMarkup [
          M (arb :: Gen PageRoomsOverview)
        , M (arb :: Gen PageIdeasOverview)
        , M (arb :: Gen PageIdeasInDiscussion)
        , M (arb :: Gen PageTopicOverviewRefinementPhase)
        , M (arb :: Gen PageTopicOverviewAssessmentPhase)
        , M (arb :: Gen PageTopicOverviewVotingPhase)
        , M (arb :: Gen PageTopicOverviewResultPhase)
        , M (arb :: Gen PageTopicOverviewDelegations)
        , M (arb :: Gen PageIdeaDetailNewIdeas)
        , M (arb :: Gen PageIdeaDetailRefinementPhase)
        , M (arb :: Gen PageIdeaDetailAssessmentPhase)
        , M (arb :: Gen PageIdeaDetailVotingPhase)
        , M (arb :: Gen PageIdeaDetailMoveIdeaToTopic)
        , M (arb :: Gen PageIdeaDetailFeasibleNotFeasible)
        , M (arb :: Gen PageIdeaDetailWinner)
        , M (arb :: Gen PageCreateIdea)
        , M (arb :: Gen PageEditIdea)
        , M (arb :: Gen PageUserProfileCreateIdeas)
        , M (arb :: Gen PageUserProfileDelegatedVotes)
        , M (arb :: Gen PageUserSettings)
        , M (arb :: Gen PageCreateTopic)
        , M (arb :: Gen PageCreateTopicAddIdeas)
        , M (arb :: Gen PageAdminSettingsDurationsAndQuorum)
        , M (arb :: Gen PageAdminSettingsGroupsAndPermissions)
        , M (arb :: Gen PageAdminSettingsUserCreateAndImport)
        , M (arb :: Gen PageAdminSettingsEventsProtocol)
        , M (arb :: Gen PageDelegateVote)
        , M (arb :: Gen PageDelegationNetwork)
        , M (arb :: Gen PageStaticImprint)
        , M (arb :: Gen PageStaticTermsOfUse)
        , M (arb :: Gen PageHomeWithLoginPrompt)
        , M (PageIdea    <$> arb)
        , M (PageComment <$> arb)
        ]
    where
        arb :: Arbitrary a => Gen a
        arb = arbitrary

data MarkupGen where
    M :: (Show m, Typeable m, ToMarkup m) => Gen m -> MarkupGen 

-- | Checks if the markup rendering does not contains bottoms.
renderMarkup (M g) = describe (show $ typeOf g) .
    it "renders" . property . forAll g $ \pageSource -> length (renderHtml (toMarkup pageSource)) > 0

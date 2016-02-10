{-# LANGUAGE GADTs #-}
module Frontend.HtmlSpec where

import Arbitrary
import Frontend.Html

import Control.Applicative ((<$>))
import Data.Typeable (Typeable, typeOf)
import Lucid (ToHtml, toHtml, renderText)
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Arbitrary(..), Gen, Testable, forAll, property)

import qualified Data.Text.Lazy as LT


spec :: Spec
spec = do
    mapM_ renderMarkup [
          H (arb :: Gen PageRoomsOverview)
        , H (arb :: Gen PageIdeasOverview)
        , H (arb :: Gen PageIdeasInDiscussion)
        , H (arb :: Gen PageTopicOverviewRefinementPhase)
        , H (arb :: Gen PageTopicOverviewAssessmentPhase)
        , H (arb :: Gen PageTopicOverviewVotingPhase)
        , H (arb :: Gen PageTopicOverviewResultPhase)
        , H (arb :: Gen PageTopicOverviewDelegations)
        , H (arb :: Gen PageIdeaDetailNewIdeas)
        , H (arb :: Gen PageIdeaDetailRefinementPhase)
        , H (arb :: Gen PageIdeaDetailAssessmentPhase)
        , H (arb :: Gen PageIdeaDetailVotingPhase)
        , H (arb :: Gen PageIdeaDetailMoveIdeaToTopic)
        , H (arb :: Gen PageIdeaDetailFeasibleNotFeasible)
        , H (arb :: Gen PageIdeaDetailWinner)
        , H (arb :: Gen PageCreateIdea)
        , H (arb :: Gen PageEditIdea)
        , H (arb :: Gen PageUserProfileCreateIdeas)
        , H (arb :: Gen PageUserProfileDelegatedVotes)
        , H (arb :: Gen PageUserSettings)
        , H (arb :: Gen PageCreateTopic)
        , H (arb :: Gen PageCreateTopicAddIdeas)
        , H (arb :: Gen PageAdminSettingsDurationsAndQuorum)
        , H (arb :: Gen PageAdminSettingsGroupsAndPermissions)
        , H (arb :: Gen PageAdminSettingsUserCreateAndImport)
        , H (arb :: Gen PageAdminSettingsEventsProtocol)
        , H (arb :: Gen PageDelegateVote)
        , H (arb :: Gen PageDelegationNetwork)
        , H (arb :: Gen PageStaticImprint)
        , H (arb :: Gen PageStaticTermsOfUse)
        , H (arb :: Gen PageHomeWithLoginPrompt)
        , H (PageIdea    <$> arb)
        , H (PageComment <$> arb)
        ]
    where
        arb :: Arbitrary a => Gen a
        arb = arbitrary

data HtmlGen where
    H :: (Show m, Typeable m, ToHtml m) => Gen m -> HtmlGen

-- | Checks if the markup rendering does not contains bottoms.
renderMarkup (H g) = describe (show $ typeOf g) .
    it "renders" . property . forAll g $ \pageSource -> LT.length (renderText (toHtml pageSource)) > 0

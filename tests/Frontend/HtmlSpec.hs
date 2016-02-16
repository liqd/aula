{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Frontend.HtmlSpec where

import Arbitrary ()
import Frontend.Core
import Frontend.Html
import Frontend.Page.CreateIdea
import Frontend.Topics

import Control.Monad.Trans.Except
import Data.Typeable (Typeable, typeOf)
import Lucid (ToHtml, toHtml, renderText)
import Test.Hspec (Spec, context, it, pendingWith)
import Test.QuickCheck (Arbitrary(..), Gen, forAll, property)
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Text.Digestive.View
import qualified Data.Text.Lazy as LT


spec :: Spec
spec = do
    context "ToHtml" $ mapM_ renderMarkup [
          H (arb :: Gen PageRoomsOverview)
        , H (arb :: Gen PageIdeasOverview)
        , H (arb :: Gen PageIdeasInDiscussion)
        , H (arb :: Gen PageTopicOverviewRefinementPhase)
        , H (arb :: Gen PageTopicOverviewJuryPhase)
        , H (arb :: Gen PageTopicOverviewVotingPhase)
        , H (arb :: Gen PageTopicOverviewResultPhase)
        , H (arb :: Gen PageTopicOverviewDelegations)
        , H (arb :: Gen PageIdeaDetailNewIdeas)
        , H (arb :: Gen PageIdeaDetailRefinementPhase)
        , H (arb :: Gen PageIdeaDetailJuryPhase)
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
    context "PageFormView" $ mapM_ testForm [
          F (arb :: Gen PageCreateIdea)
        ]
    where
        arb :: Arbitrary a => Gen a
        arb = arbitrary

data HtmlGen where
    H :: (Show m, Typeable m, ToHtml m) => Gen m -> HtmlGen

-- | Checks if the markup rendering does not contains bottoms.
renderMarkup :: HtmlGen -> Spec
renderMarkup (H g) =
    it (show $ typeOf g) . property . forAll g $ \pageSource ->
        LT.length (renderText (toHtml pageSource)) > 0

data FormGen where
    F :: (Show m, Typeable m, FormPageView m) => Gen m -> FormGen

testForm :: FormGen -> Spec
testForm fg = renderForm fg >> postToForm fg

-- | Checks if the form rendering does not contains bottoms and
-- the view has all the fields defined for GET form creation.
renderForm :: FormGen -> Spec
renderForm (F g) =
    it (show (typeOf g) ++ " (show empty form)") . property . forAll g $ \page -> monadicIO $ do
        len <- run . failOnError $ do
            v <- getForm "" $ makeForm page
            return $ LT.length (renderText $ formPage v "formAction" page)
        assert (len > 0)

failOnError :: ExceptT ServantErr IO a -> IO a
failOnError = fmap (either (error . show) id) . runExceptT

-- | Checks if the form processes valid and invalid input a valid output and an error page, resp.
postToForm :: FormGen -> Spec
postToForm (F g) =
    it (show (typeOf g) ++ " (process form input)") $
        pendingWith "not implemented."
    where
        failOnError = either (error . show) id

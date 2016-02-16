{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Frontend.HtmlSpec where

import Arbitrary ()
import Frontend.Core
import Frontend.Html
import Frontend.Page.CreateIdea
import Frontend.Topics

import Control.Monad.Identity
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.String.Conversions
import Data.Typeable (Typeable, typeOf)
import Lucid (ToHtml, toHtml, renderText)
import Servant.Server.Internal.ServantErr
import Test.Hspec (Spec, context, it, pendingWith)
import Test.QuickCheck (Arbitrary(..), Gen, forAll, property)
import Test.QuickCheck.Monadic (assert, monadicIO, run, pick)
import Text.Digestive.Form
import Text.Digestive.Types
import Text.Digestive.View

import qualified Data.Text.Lazy as LT

import Types


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
    F :: ( r ~ FormPageResult m
         , Show m, Typeable m, FormPageView m
         , Show r, Eq r, Arbitrary r, PayloadToEnv r
         ) => Gen m -> FormGen

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
postToForm (F g) = do
    it (show (typeOf g) ++ " (process valid forms)") . property . monadicIO $ do
        page <- pick g
        payload <- pick (arbFormPageResult page)

        let env :: Env (ExceptT ServantErr IO) = payloadToEnv payload
            frm = makeForm page
        (_, Just payload') <- run . failOnError $ postForm "" frm (\_ -> pure env)
        assert (payload' == payload)  -- FIXME: can we use shouldBe here?

    it (show (typeOf g) ++ " (process *in*valid form input)") $
        pendingWith "not implemented."


arbFormPageResult :: (r ~ FormPageResult p, FormPageView p, Arbitrary r, Show r) => p -> Gen r
arbFormPageResult _ = arbitrary

class PayloadToEnv a where
    payloadToEnv :: a -> Env (ExceptT ServantErr IO)

instance PayloadToEnv ProtoIdea where
    payloadToEnv (ProtoIdea t (Markdown d) c) = \case
        ["", "title"]         -> pure [TextInput t]
        ["", "idea-text"]     -> pure [TextInput d]
        ["", "idea-category"] -> pure [TextInput . cs . showCategoryValue $ c]
        bad -> error $ "instance PayloadToEnv ProtoIdea: " ++ show bad
      -- FIXME: reduce boilerplate?

showCategoryValue :: Category -> String
showCategoryValue cat = case lookup cat categoryValues of Just s -> s

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC -Wall -Werror -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}

module Frontend.HtmlSpec where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.List
import Data.Maybe (fromMaybe)
import Data.String
import Data.String.Conversions
import Data.Typeable (Typeable, typeOf)
import Lucid (Html, ToHtml, toHtml, renderText)
import Servant (unNat)
import Servant.Server.Internal.ServantErr
import Test.Hspec (Spec, context, it, pendingWith, shouldBe)
import Test.QuickCheck (Arbitrary(..), Gen, forAll, property)
import Test.QuickCheck.Monadic (assert, monadicIO, run, pick)
import Text.Digestive.Types
import Text.Digestive.View

import qualified Data.Text.Lazy as LT

import Action
import Arbitrary ()
import Data.UriPath (absoluteUriPath)
import Frontend.Page
import Persistent
import Types


----------------------------------------------------------------------
-- list all types for testing

spec :: Spec
spec = do
    context "ToHtml" $ mapM_ renderMarkup [
          H (arb :: Gen PageRoomsOverview)
        , H (arb :: Gen PageIdeasOverview)
        , H (arb :: Gen PageIdeasInDiscussion)
        , H (arb :: Gen ViewTopic)
        , H (arb :: Gen ViewIdea)
        , H (arb :: Gen PageUserProfileCreatedIdeas)
        , H (arb :: Gen PageUserProfileDelegatedVotes)
        , H (arb :: Gen PageAdminSettingsGroupsAndPermissions)
--        , H (arb :: Gen PageAdminSettingsUserCreateAndImport)
        , H (arb :: Gen PageAdminSettingsEventsProtocol)
        , H (arb :: Gen PageDelegateVote)
        , H (arb :: Gen PageDelegationNetwork)
        , H (arb :: Gen PageStaticImprint)
        , H (arb :: Gen PageStaticTermsOfUse)
        , H (arb :: Gen PageLogout)
        , H (PageComment <$> arb)
        ]
    context "PageFormView" $ mapM_ testForm [
          F (arb :: Gen CreateIdea)
        , F (arb :: Gen EditIdea)
        , F (arb :: Gen PageHomeWithLoginPrompt)
    --  , F (arb :: Gen CreateTopic) FIXME
        , F (arb :: Gen PageUserSettings)
    --  , F (arb :: Gen MoveIdeasToTopic) FIXME
        , F (arb :: Gen PageAdminSettingsDurations)
        , F (arb :: Gen PageAdminSettingsQuorum)
        ]
    where
        arb :: Arbitrary a => Gen a
        arb = arbitrary


----------------------------------------------------------------------
-- translate form data back to form input

instance PayloadToEnv ProtoIdea where
    payloadToEnv view (ProtoIdea t (Markdown d) c) = \case
        ["", "title"]         -> pure [TextInput t]
        ["", "idea-text"]     -> pure [TextInput d]
        ["", "idea-category"] -> pure [TextInput $ selectCategoryValue "idea-category" view c]
        bad -> error $ "instance PayloadToEnv ProtoIdea: " <> show bad
      -- FIXME: reduce boilerplate?

-- | Translate a 'Category' value into the select string for the form 'Env'.
--
-- FIXME: this function should be more general.
-- FIXME: none of this is very elegant.  can we improve on it?
--
-- Text fields in forms are nice because the values in the form 'Env' contains simply the text
-- itself, as it ends up in the parsed form playload.  Selections (pull-down menus) are trickier,
-- because 'Env' maps their path to an internal representation of a reference to the selected item,
-- rather than the human-readable menu entry.
--
-- This function mimics the 'inputSelect' functions internal behavior from the
-- digestive-functors-lucid package: it extracts an enumeration of the input choices from the views,
-- constructs the form field values from that, and looks up the one whose item description matches
-- the given category value.
--
-- Since the item descriptions are available only as 'Html', not as text, and 'Html' doesn't have
-- 'Eq', we need to apply another trick and transform both the category value and the item
-- description to 'LT'.
selectCategoryValue :: ST -> View (Html ()) -> Category -> ST
selectCategoryValue ref view cat = case find test choices of Just (i, _, _) -> value i
  where
    ref'    = absoluteRef ref view
    value i = ref' <> "." <> i
    choices = fieldInputChoice ref view
    test (_, scat :: Html (), _) = showCategoryValue cat == renderText scat

    showCategoryValue :: IsString s => Category -> s
    showCategoryValue ((`lookup` categoryValues) -> Just v) = v

instance PayloadToEnv LoginFormData where
    payloadToEnv _ (LoginFormData name pass) = \case
        ["", "user"] -> pure [TextInput name]
        ["", "pass"] -> pure [TextInput pass]
        bad -> error $ "instance PayloadToEnv LoginFormData: " <> show bad

instance PayloadToEnv ProtoTopic where
    payloadToEnv _ (ProtoTopic title (Markdown desc) image _ _) = \case
        ["", "title"] -> pure [TextInput title]
        ["", "desc"]  -> pure [TextInput desc]
        ["", "image"] -> pure [TextInput image]
        [""]          -> pure [] -- FIXME: why?
        bad -> error $ "instance PayloadToEnv ProtoTopic: " <> show bad

instance PayloadToEnv [AUID Idea] where
    payloadToEnv _ ideas = \case
        ["", s] | "idea-" `isPrefixOf` (cs s :: String) -> pure [TextInput (if cs s `elem` ideas' then "on" else "off")]
        [""]                                            -> pure [] -- FIXME: why?
        bad -> error $ "instance PayloadToEnv [AUID Idea]: " <> show bad
      where
        ideas' = [ "idea-" <> show i | i <- ideas ]

instance PayloadToEnv UserSettingData where
    payloadToEnv _ (UserSettingData email oldpass newpass1 newpass2) = \case
        ["", "email"         ] -> pure [TextInput . fromMaybe "" $ fmap unEmail email]
        ["", "old-password"  ] -> pure [TextInput $ fromMaybe "" oldpass]
        ["", "new-password1" ] -> pure [TextInput $ fromMaybe "" newpass1]
        ["", "new-password2" ] -> pure [TextInput $ fromMaybe "" newpass2]
      where
        unEmail (Email e) = e

instance PayloadToEnv Durations where
    payloadToEnv _ (Durations elab vote) = \case
        ["", "elab-duration"] -> pure [TextInput (cs $ show elab)]
        ["", "vote-duration"] -> pure [TextInput (cs $ show vote)]

instance PayloadToEnv Quorums where
    payloadToEnv _ (Quorums school clss) = \case
        ["", "school-quorum"] -> pure [TextInput (cs $ show school)]
        ["", "class-quorum"]  -> pure [TextInput (cs $ show clss)]

----------------------------------------------------------------------
-- machine room

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
    it (show (typeOf g) <> " (show empty form)") . property . forAll g $ \page -> monadicIO $ do
        len <- run . failOnError $ do
            v <- getForm (absoluteUriPath $ formAction page) (makeForm page)
            return . LT.length . renderText $ formPage v "formAction" page
        assert (len > 0)

runAction :: Action a -> ExceptT ServantErr IO a
runAction action = do rp <- liftIO mkRunPersist
                      unNat (mkRunAction rp UserLoggedOut) action

failOnError :: Action a -> IO a
failOnError = fmap (either (error . show) id) . runExceptT . runAction

-- | Checks if the form processes valid and invalid input a valid output and an error page, resp.
--
-- For valid inputs, we generate an arbitrary value of the type generated by the form parser,
-- translate it back into a form 'Env' with a 'PayloadToEnv' instance, feed that into 'postForm',
-- and compare the parsed output with the generated output.
--
-- For invalid inputs, we have to go about it differently: since we don't expect to get a valid form
-- output, we generate an 'Env' directly that can contain anything expressible in a valid HTTP POST
-- request, including illegal or missing form fields, arbitrary invalid string values etc.  This
-- happens in an appropriate 'ArbitraryBadEnv' instance.  For the test to succeed, we compare the
-- errors in the view constructed by 'postForm' against the expected errors generated along with the
-- bad env.
postToForm :: FormGen -> Spec
postToForm (F g) = do
    it (show (typeOf g) <> " (process valid forms)") . property . monadicIO $ do
        page <- pick g
        payload <- pick (arbFormPageResult page)

        let frm = makeForm page
        env <- run' $ (`payloadToEnv` payload) <$> getForm "" frm

        (_, Just payload') <- run' $ postForm "" frm (\_ -> pure env)
        liftIO $ payload' `shouldBe` payload

    it (show (typeOf g) <> " (process *in*valid form input)") $
        pendingWith "not implemented."  -- FIXME
    where
        run' = run . failOnError

arbFormPageResult :: (r ~ FormPageResult p, FormPageView p, Arbitrary r, Show r) => p -> Gen r
arbFormPageResult _ = arbitrary

class PayloadToEnv a where
    payloadToEnv :: View (Html ()) -> a -> Env Action

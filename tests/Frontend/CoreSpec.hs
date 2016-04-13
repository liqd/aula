{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC -Wall -Werror -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}

module Frontend.CoreSpec where

import Control.Arrow((&&&))
import Control.Monad.Trans.Except
import Data.List
import Data.String.Conversions
import Data.Typeable (typeOf)
import Test.QuickCheck (Arbitrary(..), Gen, forAll, property)
import Test.QuickCheck.Monadic (PropertyM, assert, monadicIO, run, pick)
import Text.Digestive.Types
import Text.Digestive.View

import qualified Data.Text.Lazy as LT
import qualified Text.Digestive.Lucid.Html5 as DF

import Action
import Action.Implementation
import Arbitrary (arb, arbPhrase, schoolClasses)
import Config
import Frontend.Core
import Frontend.Page
import Types

import AulaTests


-- * list all types for testing

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
        , H (arb :: Gen PageAdminSettingsGaPUsersView)
        , H (arb :: Gen PageAdminSettingsGaPUsersCreate)
        , H (arb :: Gen PageAdminSettingsGaPClassesView)
        , H (arb :: Gen PageAdminSettingsEventsProtocol)
        , H (arb :: Gen PageDelegateVote)
        , H (arb :: Gen PageDelegationNetwork)
        , H (arb :: Gen PageStaticImprint)
        , H (arb :: Gen PageStaticTermsOfUse)
        , H (arb :: Gen PageAdminSettingsGaPClassesEdit)
        , H (arb :: Gen CommentWidget)
        ]
    context "PageFormView" $ mapM_ testForm [
--          F (arb :: Gen CreateIdea)  -- FIXME
--          F (arb :: Gen Frontend.Page.EditIdea)  -- FIXME
          F (arb :: Gen CommentIdea)
--      , F (arb :: Gen PageHomeWithLoginPrompt) -- FIXME cannot fetch the password back from the payload
        , F (arb :: Gen CreateTopic)
        , F (arb :: Gen PageUserSettings)
        , F (arb :: Gen Frontend.Page.EditTopic)
        , F (arb :: Gen PageAdminSettingsDurations)
        , F (arb :: Gen PageAdminSettingsQuorum)
--        , F (arb :: Gen PageAdminSettingsGaPUsersEdit) -- FIXME
        ]


-- * translate form data back to form input

-- | Translate a value into the select string for the form 'Env'.
--
-- FIXME: none of this is very elegant.  can we improve on it?
-- FIXME: this function does not work for complex ADTs. E.g: 'SchoolClass Int String'
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
selectValue :: Eq a => ST -> View (Html ()) -> [(a, LT.Text)] -> a -> ST
selectValue ref v xs x = case find test choices of Just (i, _, _) -> value i
  where
    ref'    = absoluteRef ref v
    value i = ref' <> "." <> i
    choices = fieldInputChoice ref v
    test (_, sx :: Html (), _) = showValue x == renderText sx
    showValue ((`lookup` xs) -> Just y) = y

-- | In order to be able to call 'payloadToEnvMapping, define a `PayloadToEnv' instance.
class PayloadToEnv a where
    payloadToEnvMapping :: View (Html ()) -> a -> ST -> Action [FormInput]

-- | When context dependent data is constructed via forms with the 'pure' combinator
-- in the form description, in the digestive functors libarary an empty path will
-- be generated. Which is not an issue here. This functions guards against that with
-- the @[""]@ case.
--
-- Example:
--
-- >>> ProtoIdea <$> ... <*> pure ScoolSpave <*> ...
payloadToEnv :: (PayloadToEnv a) => View (Html ()) -> a -> Env Action
payloadToEnv _ _ [""]       = pure []
payloadToEnv v a ["", path] = payloadToEnvMapping v a path

instance PayloadToEnv ProtoIdea where
    payloadToEnvMapping _v (ProtoIdea t (Markdown d) c _is) = \case
        "title"         -> pure [TextInput t]
        "idea-text"     -> pure [TextInput d]
        "idea-category" -> pure [TextInput $ fromMaybe nil (cs . show . fromEnum <$> c)]

instance PayloadToEnv User where
    payloadToEnvMapping _ u = \case
        "user" -> pure [TextInput $ u ^. userLogin . fromUserLogin]
        "pass" -> pure []

ideaCheckboxValue iids path =
    if path `elem` (("idea-" <>) . show <$> iids)
        then "on"
        else "off"

instance PayloadToEnv ProtoTopic where
    payloadToEnvMapping _ (ProtoTopic title (Markdown desc) image _ iids _) path'
        | "idea-" `isPrefixOf` path = pure [TextInput $ ideaCheckboxValue iids path]
        | path == "title" = pure [TextInput title]
        | path == "desc"  = pure [TextInput desc]
        | path == "image" = pure [TextInput image]
      where
        path :: String = cs path'

instance PayloadToEnv EditTopicData where
    payloadToEnvMapping _ (EditTopicData title (Markdown desc) iids) path'
        | "idea-" `isPrefixOf` path = pure [TextInput $ ideaCheckboxValue iids path]
        | path == "title"           = pure [TextInput title]
        | path == "desc"            = pure [TextInput desc]
      where
        path :: String = cs path'

instance PayloadToEnv UserSettingData where
    payloadToEnvMapping _ (UserSettingData email oldpass newpass1 newpass2) = \case
        "email"         -> pure [TextInput $ email ^. _Just . re emailAddress]
        "old-password"  -> pure [TextInput $ fromMaybe "" oldpass]
        "new-password1" -> pure [TextInput $ fromMaybe "" newpass1]
        "new-password2" -> pure [TextInput $ fromMaybe "" newpass2]

instance PayloadToEnv Durations where
    payloadToEnvMapping _ (Durations elab vote) = \case
        "elab-duration" -> pure [TextInput (cs . show . fromDurationDays $ elab)]
        "vote-duration" -> pure [TextInput (cs . show . fromDurationDays $ vote)]

instance PayloadToEnv Quorums where
    payloadToEnvMapping _ (Quorums school clss) = \case
        "school-quorum" -> pure [TextInput (cs $ show school)]
        "class-quorum"  -> pure [TextInput (cs $ show clss)]

instance PayloadToEnv EditUserPayload where
    payloadToEnvMapping v (EditUserPayload r c) = \case
        "user-role"  -> pure [TextInput $ selectValue "user-role" v roleSelectionChoices r]
        -- FIXME: Selection does not work for composite types like school class.
        "user-class" -> pure [TextInput $ selectValue "user-class" v classes c]
      where
        classes = (id &&& cs . view className) <$> schoolClasses

instance PayloadToEnv Document where
    payloadToEnvMapping _ (Markdown comment) = \case
        "comment-text" -> pure [TextInput comment]


-- * machine room

data HtmlGen where
    H :: (Show m, Typeable m, ToHtml m) => Gen m -> HtmlGen

-- | Checks if the markup rendering does not contains bottoms.
renderMarkup :: HtmlGen -> Spec
renderMarkup (H g) =
    it (show $ typeOf g) . property . forAll g $ \pageSource ->
        LT.length (renderText (toHtml pageSource)) > 0

data FormGen where
    F :: ( r ~ FormPagePayload m
         , Show m, Typeable m, FormPage m
         , Show r, Eq r, Arbitrary r, PayloadToEnv r
         , ArbFormPagePayload m
         ) => Gen m -> FormGen

testForm :: FormGen -> Spec
testForm fg = renderForm fg >> postToForm fg

-- | Checks if the form rendering does not contains bottoms and
-- the view has all the fields defined for GET form creation.
renderForm :: FormGen -> Spec
renderForm (F g) =
    it (show (typeOf g) <> " (show empty form)") . property . forAll g $ \page -> monadicIO $ do
        len <- runFailOnError $ do
            v <- getForm (absoluteUriPath . relPath $ formAction page) (makeForm page)
            return . LT.length . renderText $ formPage v (DF.form v "formAction") page
        assert (len > 0)

runFailOnError :: Action a -> PropertyM IO a
runFailOnError action = run $ do
    cfg <- readConfig DontWarnMissing
    let env :: ActionEnv = ActionEnv (error "Dummy RunPersist") cfg
    fmap (either (error . show) id) . runExceptT . unNat (mkRunAction env) $ action

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
        payload <- pick (arbFormPagePayload page)

        let frm = makeForm page
        env <- runFailOnError $ (`payloadToEnv` payload) <$> getForm "" frm

        (_, Just payload') <- runFailOnError $ postForm "" frm (\_ -> pure env)
        liftIO $ payload' `shouldBe` payload

    it (show (typeOf g) <> " (process *in*valid form input)") $
        pendingWith "not implemented."  -- FIXME

-- | Arbitrary test data generation for the 'FormPagePayload' type.
--
-- In some cases the arbitrary data generation depends on the 'Page' context
-- and the 'FormPagePayload' has to compute data from the context.
class FormPage p => ArbFormPagePayload p where
    arbFormPagePayload :: (r ~ FormPagePayload p, FormPage p, Arbitrary r, Show r) => p -> Gen r

instance ArbFormPagePayload CreateIdea where
    arbFormPagePayload (CreateIdea location) = set protoIdeaLocation location <$> arbitrary

instance ArbFormPagePayload Frontend.Page.EditIdea where
    arbFormPagePayload (Frontend.Page.EditIdea idea) =
        set protoIdeaLocation (idea ^. ideaLocation) <$> arbitrary

instance ArbFormPagePayload CommentIdea where
    arbFormPagePayload _ = arbitrary

instance ArbFormPagePayload PageAdminSettingsQuorum where
    arbFormPagePayload _ = arbitrary

instance ArbFormPagePayload PageAdminSettingsDurations where
    arbFormPagePayload _ = arbitrary

instance ArbFormPagePayload PageUserSettings where
    arbFormPagePayload _ = arbitrary

instance ArbFormPagePayload PageHomeWithLoginPrompt where
    arbFormPagePayload _ = arbitrary

instance ArbFormPagePayload CreateTopic where
    arbFormPagePayload (CreateTopic space ideas _timestamp) =
            set protoTopicIdeaSpace space
          . set protoTopicIdeas (map (^. _Id) ideas)
        <$> arbitrary

instance ArbFormPagePayload Frontend.Page.EditTopic where
    arbFormPagePayload (Frontend.Page.EditTopic _space _topicid ideas) =
        EditTopicData
        <$> arbPhrase
        <*> arbitrary
        -- FIXME: Generate a sublist from the given ideas
        -- Ideas should be a set which contains only once one idea. And the random
        -- result generation should select from those ideas only.
        <*> pure (view _Id <$> ideas)

instance ArbFormPagePayload PageAdminSettingsGaPUsersEdit where
    arbFormPagePayload _ = arbitrary

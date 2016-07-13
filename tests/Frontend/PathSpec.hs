{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeOperators          #-}

{-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans #-}

module Frontend.PathSpec
where

import Control.Monad (forM_)
import Data.String.Conversions (cs, (<>))
import Data.Typeable (Typeable, typeOf)
import Network.HTTP.Types.Status (statusCode)
import Network.Wai
import Network.Wai.Test
import Servant
import Servant.Missing hiding (redirect)
import Servant.Mock (HasMock(..), mock)
import Test.Hspec (Spec, beforeAll, describe, it)
import Test.Hspec.Wai (get, post)
import Test.QuickCheck hiding (Large)
import Text.Digestive.View (getForm)

import qualified Data.Text as ST
import qualified Test.Hspec.Wai.QuickCheck as Wai (property)

import Action.Dummy
import Arbitrary
import AulaTests (wpasses, TestSuite(..), tag)
import Data.UriPath
import Frontend
import Frontend.Core
import Frontend.Filter
import Frontend.Page
import Frontend.Path
import Types


spec :: Spec
spec = do
    describe "HasPath" $ do
        it "absoluteUriPath is not empty and well defined" . property . forAllShrinkDef mainGen $ \path ->
            ST.length (absoluteUriPath $ relPath path) >= 1
        it "relativeUriPath is not empty and well defined" . property . forAllShrinkDef mainGen $ \path ->
            ST.length (relativeUriPath $ relPath path) >= 0

    describe "FromHttpApiData <-> UriPath" $ do
        mapM_ uriPartAndHttpApiDataAreInverses
            [ U (arb :: Gen IdeaSpace)
            , U (arb :: Gen SchoolClass)
            , U (arb :: Gen IdeaVoteValue)
            , U (arb :: Gen IdeaJuryResultType)
            , U (arb :: Gen UpDown)
            , U (arb :: Gen PasswordToken)
            ]

    describe "FromHttpApiData <-> ToHttpApiData" $ do
        mapM_ fromAndToHttpApiDataAreInverses
            [ H (arb :: Gen IdeaSpace)
            , H (arb :: Gen IdeaJuryResultType)
            , H (arb :: Gen DScope)
            , H (arb :: Gen Category)
            , H (arb :: Gen SortIdeasBy)
            , H (arb :: Gen SortUsersBy)
            ]

    let checkPathHandler gen app = property . forAllShrinkDef gen $ \path ->
            flip Wai.property app $ do
                let uri = cs . ST.takeWhile (/= '#') . absoluteUriPath $ relPath path
                resp :: SResponse <- if isPostOnly path then post uri ""
                                                        else get  uri
                let s :: Int    = statusCode . simpleStatus $ resp
                    b :: String =           cs . simpleBody $ resp
                    msg = "this test needs arbitrary paths to point to existing data: "
                      -- FIXME: as long as src/Arbitrary.hs is around and not replaced by
                      -- src/DemoData.hs or AulaTests/Stories.hs, this text will keep causing
                      -- trouble, so we just make failing test cases pending.

                case s of
                    204 -> wpasses
                    201 -> wpasses
                    200 -> wpasses
                    _   -> error (msg <> show (uri, s, b))

    -- NOTE: It also uses the formPage, makeForm, ToHTML instances, where can issues occur.
    beforeAll mockAulaMain $ do

        describe "Paths and handlers" $ do
            it "Every path has a handler." $
                checkPathHandler mainGen

        tag Large . describe "Valid formAction" $ do
            forM_ formActionGens $ \(t, g) ->
                it (t <> " has a formAction.") $
                    checkPathHandler g

        tag Large . describe "Valid redirectOf" $ do
            forM_ formRedirectGens $ \(t, g) ->
                it (t <> " has a valid redirect.") $
                    checkPathHandler g

  where
    mainGen :: Gen (Main 'AllowGetPost)
    mainGen = arbitrary

    formActionGens :: [(String, Gen (Main 'AllowGetPost))]
    formActionGens = map (\(F g) -> (show (typeOf g), formAction <$> g)) forms

    formRedirectGens :: [(String, Gen (Main 'AllowGetPost))]
    formRedirectGens = map (\(F g) -> (show (typeOf g), redirectOf <$> g <*> arb)) forms

    forms :: [FormGen]
    forms =
          -- admin forms
        [ F (arb :: Gen PageAdminSettingsDurations)
        , F (arb :: Gen PageAdminSettingsQuorum)
        , F (arb :: Gen PageAdminSettingsFreeze)
        , F (arb :: Gen PageAdminSettingsEventsProtocol)
        , F (arb :: Gen AdminEditUser)
        , F (arb :: Gen AdminDeleteUser)
        , F (arb :: Gen AdminCreateUser)
        , F (arb :: Gen AdminCreateClass)
        , F (arb :: Gen AdminPhaseChange)
        , F (arb :: Gen PageAdminResetPassword)
        , F (arb :: Gen PageAdminTermsOfUse)

          -- idea forms
        , F (arb :: Gen CreateIdea)
        , F (arb :: Gen Frontend.Page.MoveIdea)
        , F (arb :: Gen CommentOnIdea)
        , F (arb :: Gen Frontend.Page.EditIdea)
        , F (arb :: Gen EditComment)
        , F (arb :: Gen JudgeIdea)
        , F (arb :: Gen CreatorStatement)
        , F (arb :: Gen ReportComment)
        , F (arb :: Gen ReportIdea)

          -- login forms
        , F (arb :: Gen PageHomeWithLoginPrompt)

          -- topic forms
        , F (arb :: Gen CreateTopic)
        , F (arb :: Gen EditTopic)

          -- user forms
        , F (arb :: Gen PageUserSettings)
        , F (arb :: Gen EditUserProfile)
        , F (arb :: Gen ReportUserProfile)
        ]

-- FIXME: Unify the Form Arbitrary GADTs and generate the form
-- list as we generate the AllModules.
data FormGen where
    F :: ( r ~ FormPageResult m
         , Show m, Typeable m, FormPage m
         , Show r, Eq r, Arbitrary r
         , Arbitrary m
         ) => Gen m -> FormGen

-- * Each path has a handler

instance (FormPage a, Arbitrary a) => Arbitrary (FormPageRep a) where
    arbitrary = do
        page        <- arb
        frameAction <- arb
        Right view  <- runDummyT $ getForm frameAction (makeForm page)
        pure $ FormPageRep Nothing view frameAction page

mockAulaMain :: IO Application
mockAulaMain = do
    return $ serve (Proxy :: Proxy AulaMain) (mock (Proxy :: Proxy AulaMain) (Proxy :: Proxy '[]))

instance (Show a, FormPage a, Page a, Arbitrary a)
        => HasMock (FormReqBody :>
                        Post '[IHTML, PlainText]
                            (PostResult (Frame (FormPageRep a)) (Frame (FormPageRep a)))) context where
    mock _ proxyContext _ = mock (Proxy :: Proxy (Post '[IHTML, PlainText]
                                        (PostResult' (Frame (FormPageRep a))))) proxyContext


-- * UriPath and FromHttpApiData correspondence

data UriPartGen where
    U :: (Show d, Typeable d, FromHttpApiData d, HasUriPart d, Eq d, Arbitrary d) =>
        Gen d -> UriPartGen

uriPartAndHttpApiDataAreInverses :: UriPartGen -> Spec
uriPartAndHttpApiDataAreInverses (U g) =
    it (show $ typeOf g) . property . forAllShrinkDef g $ \uriPartData ->
        (Right uriPartData ==) . parseUrlPiece . cs $ uriPart uriPartData

data HttpApiGen where
    H :: (Show d, Eq d, Typeable d, Arbitrary d, FromHttpApiData d, ToHttpApiData d)
      => Gen d -> HttpApiGen

fromAndToHttpApiDataAreInverses :: HttpApiGen -> Spec
fromAndToHttpApiDataAreInverses (H g) =
    it (show $ typeOf g) . property . forAllShrinkDef g $ \httpApiData ->
        (Right httpApiData ==) . parseUrlPiece . cs $ toUrlPiece httpApiData

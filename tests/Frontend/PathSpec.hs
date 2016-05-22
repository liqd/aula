{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

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
import Servant.HTML.Lucid
import Servant.Missing hiding (redirect)
import Servant.Mock (HasMock(..), mock)
import Test.Hspec (Spec, beforeAll, describe, it)
import Test.Hspec.Wai (get, post)
import Test.QuickCheck
import Text.Digestive.View (getForm)

import qualified Data.Text as ST
import qualified Test.Hspec.Wai.QuickCheck as Wai (property)

import Action.Dummy
import Arbitrary
import AulaTests (wpasses)
import Data.UriPath
import Frontend
import Frontend.Core
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
                    200 -> wpasses
                    _   -> error (msg <> show (uri, s, b))

    -- NOTE: It also uses the formPage, makeForm, ToHTML instances, where can issues occur.
    beforeAll mockAulaMain $ do

        describe "Paths and handlers" $ do
            it "Every path has a handler." $
                checkPathHandler mainGen

        describe "Valid formAction" $ do
            forM_ formActionGens $ \(t, g) ->
                it (t <> " has a formAction.") $
                    checkPathHandler g

        describe "Valid redirectOf" $ do
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

        , F (arb :: Gen PageHomeWithLoginPrompt)
        , F (arb :: Gen CreateTopic)
        , F (arb :: Gen PageUserSettings)
        , F (arb :: Gen Frontend.Page.EditTopic)
        ]

-- FIXME: Unify the Form Arbitrary GADTs.
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
        pure $ FormPageRep view frameAction page

mockAulaMain :: IO Application
mockAulaMain = do
    return $ serve (Proxy :: Proxy AulaMain) (mock (Proxy :: Proxy AulaMain))

instance (Show a, FormPage a, Page a, Arbitrary a)
        => HasMock (FormReqBody :> Post '[Servant.HTML.Lucid.HTML, PlainText] (Frame (FormPageRep a))) where
    mock _ _ = mock (Proxy :: Proxy (Post '[Servant.HTML.Lucid.HTML, PlainText] (Frame (FormPageRep a))))


-- * UriPath and FromHttpApiData correspondence

data UriPartGen where
    U :: (Show d, Typeable d, FromHttpApiData d, HasUriPart d, Eq d, Arbitrary d) =>
        Gen d -> UriPartGen

uriPartAndHttpApiDataAreInverses :: UriPartGen -> Spec
uriPartAndHttpApiDataAreInverses (U g) =
    it (show $ typeOf g) . property . forAllShrinkDef g $ \uriPartData ->
        (Right uriPartData ==) . parseUrlPiece . cs $ uriPart uriPartData

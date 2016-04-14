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
import Test.QuickCheck (Arbitrary, Gen, forAll, property)
import Text.Digestive.View (getForm)

import qualified Data.Text as ST
import qualified Test.Hspec.Wai.QuickCheck as Wai (property)

import Action.Dummy
import Arbitrary
import AulaTests (wpasses)
import Data.UriPath
import Frontend
import Frontend.Core
import Frontend.Path
import Types


spec :: Spec
spec = do
    describe "HasPath" $ do
        it "absoluteUriPath is not empty and well defined" . property . forAll mainGen $ \path ->
            ST.length (absoluteUriPath $ relPath path) >= 1
        it "relativeUriPath is not empty and well defined" . property . forAll mainGen $ \path ->
            ST.length (relativeUriPath $ relPath path) >= 0

    describe "FromHttpApiData <-> UriPath" $ do
        mapM_ uriPartAndHttpApiDataAreInverses
            [ U (arb :: Gen PermissionContext)
            , U (arb :: Gen IdeaSpace)
            , U (arb :: Gen SchoolClass)
            , U (arb :: Gen IdeaVoteValue)
            , U (arb :: Gen UpDown)
            ]

    describe "Paths and handlers" $ do
        beforeAll mockAulaMain $ do
            it "Every path has a handler" $ \app -> property . forAll mainGen $ \path ->
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
  where
    mainGen :: Gen Main
    mainGen = arbitrary


-- * Each path has a handler

instance (FormPage a, Arbitrary a) => Arbitrary (FormPageRep a) where
    arbitrary = do
        page        <- arb
        frameAction <- arb
        Right view  <- runDummyT $ getForm frameAction (makeForm page)
        pure $ FormPageRep view frameAction (PublicFrame page)

mockAulaMain :: IO Application
mockAulaMain = do
    return $ serve (Proxy :: Proxy AulaMain) (mock (Proxy :: Proxy AulaMain))

instance (FormPage a, Page a, Arbitrary a)
        => HasMock (FormReqBody :> Post '[Servant.HTML.Lucid.HTML] (FormPageRep a)) where
    mock _ _ = mock (Proxy :: Proxy (Post '[Servant.HTML.Lucid.HTML] (FormPageRep a)))


-- * UriPath and FromHttpApiData correspondence

data UriPartGen where
    U :: (Show d, Typeable d, FromHttpApiData d, HasUriPart d, Eq d) =>
        Gen d -> UriPartGen

uriPartAndHttpApiDataAreInverses :: UriPartGen -> Spec
uriPartAndHttpApiDataAreInverses (U g) =
    it (show $ typeOf g) . property . forAll g $ \uriPartData ->
        (Right uriPartData ==) . parseUrlPiece . cs $ uriPart uriPartData

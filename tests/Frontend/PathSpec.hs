{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans #-}

module Frontend.PathSpec
where

import Control.Monad.IO.Class
import Data.String.Conversions (cs)
import Data.Typeable (Typeable, typeOf)
import Test.Hspec (Spec, beforeAll, describe, it, pending)
import Test.QuickCheck (Arbitrary, Gen, forAll, property)
import qualified Data.Text as ST
import Text.Digestive.View (getForm)

import Arbitrary
import Action.Dummy
import Data.UriPath
import Frontend
import Frontend.Core
import Frontend.Path
import Types

import Servant
import Servant.HTML.Lucid
import Servant.Mock (HasMock(..), mock)
import Servant.Missing hiding (redirect)
import Network.Wai

import Test.Hspec.Wai (get, post, shouldRespondWith)
import qualified Test.Hspec.Wai.QuickCheck as Wai (property)

isBrokenPath :: Main -> Bool
isBrokenPath = \case
    IdeaPath _ m ->
        case m of
            OnComment _ _ cm ->
                case cm of
                    -- Does not work well on random paths
                    ReportComment -> True
                    ViewComment -> True
                    _ -> False
            _ -> False

    _ -> False

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
                    let uri = cs . absoluteUriPath $ relPath path
                    if isBrokenPath path then
                        liftIO pending
                    else if isPostOnly path then
                        post uri "" `shouldRespondWith` 204
                    else
                        get  uri    `shouldRespondWith` 200

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

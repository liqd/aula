{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Frontend.PathSpec
where

import Data.String.Conversions (cs)
import Data.Typeable (Typeable, typeOf)
import Servant.API (FromHttpApiData(parseUrlPiece))
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Gen, forAll, property)
import qualified Data.Text as ST

import Arbitrary
import Data.UriPath
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
            ]
  where
    mainGen :: Gen Main
    mainGen = arbitrary


-- * UriPath and FromHttpApiData correspondence

data UriPartGen where
    U :: (Show d, Typeable d, FromHttpApiData d, HasUriPart d, Eq d) =>
        Gen d -> UriPartGen

uriPartAndHttpApiDataAreInverses :: UriPartGen -> Spec
uriPartAndHttpApiDataAreInverses (U g) =
    it (show $ typeOf g) . property . forAll g $ \uriPartData ->
        (Right uriPartData ==) . parseUrlPiece . cs $ uriPart uriPartData

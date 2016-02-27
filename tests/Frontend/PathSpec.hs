{-# OPTIONS_GHC -Wall -Werror #-}

module Frontend.PathSpec
where

import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Gen, forAll, property)
import qualified Data.Text as ST

import Arbitrary
import Data.UriPath
import Frontend.Path

spec :: Spec
spec = do
    describe "HasPath" $ do
        it "absoluteUriPath is not empty and well defined" . property . forAll mainGen $ \path ->
            ST.length (absoluteUriPath $ relPath path) >= 1
        it "relativeUriPath is not empty and well defined" . property . forAll mainGen $ \path ->
            ST.length (relativeUriPath $ relPath path) >= 0
  where
    mainGen :: Gen Main
    mainGen = arbitrary

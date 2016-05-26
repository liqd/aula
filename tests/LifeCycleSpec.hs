
{-# OPTIONS_GHC -Wall -Werror #-}

module LifeCycleSpec
where

import Data.List (intersect)
import Test.Hspec (Spec, it)
import Test.QuickCheck.Property

import Arbitrary
import AulaTests (TestSuite(..), tag)
import LifeCycle


spec :: Spec
spec = do
    tag Large . it "sameItemDoesNotAppear as clickable and grayed out in ideaCapabilities" . property .
        forAllShrinkDef arb $ \userId ->
        forAllShrinkDef arb $ \role ->
        forAllShrinkDef arb $ \idea ->
        forAllShrinkDef arb $ \phase ->
            sameItemDoesNotAppearDifferently
            $ ideaCapabilities userId role idea phase
  where
    swapSide (Clickable a) = GrayedOut a
    swapSide (GrayedOut a) = Clickable a

    sameItemDoesNotAppearDifferently :: [Clickable Capability] -> Bool
    sameItemDoesNotAppearDifferently cs =
        let x = (swapSide <$> cs) `intersect` cs
        in null x || error (show x)

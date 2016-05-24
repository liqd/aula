
{-# OPTIONS_GHC -Wall -Werror #-}

module LifeCycleSpec
where

import Data.List (intersect)
import Data.Monoid ((<>))
import Test.Hspec (Spec, it)
import Test.QuickCheck.Property

import Arbitrary
import AulaTests (TestSuite(..), tag)
import LifeCycle


spec :: Spec
spec = do
    tag Large . it "ideaCapabilities is subset of ideaCapabilitiesInAllPhase" . property .
        forAllShrinkDef arb $ \userId ->
        forAllShrinkDef arb $ \role ->
        forAllShrinkDef arb $ \idea ->
        forAllShrinkDef arb $ \phase ->
            let caps = ideaCapabilities userId role idea phase
                allCaps = ideaCapabilitiesInAllPhase userId role idea
            in case (caps, allCaps) of
                ([], [])   -> property True -- Partition: both empty
                ([], _)    -> property True -- Partition: actual caps empty, check is not necessary.
                p@(cs, as) -> showValue p $ cs `subsetOf` as -- Partition: real subset

subsetOf :: Eq a => [a] -> [a] -> Bool
subsetOf as bs = not (null (as `intersect` bs))

showValue :: (Show a, Testable prop) => a -> prop -> Property
showValue x = mapResult (\prop -> prop { reason = reason prop <> " " <> show x })

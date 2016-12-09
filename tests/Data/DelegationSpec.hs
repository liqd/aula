{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ViewPatterns         #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Data.DelegationSpec where

import Data.Function (on)
import Data.List (sort, nubBy)
import Test.Hspec
import Test.QuickCheck

import Arbitrary ()
import Data.Delegation


spec :: Spec
spec = do
    describe "toList, fromList" $ do
        it "are inverses" . property . forAllShrink arbitrary shrink $ do
            \d -> fromList (toList d) `shouldBe` d
        it "are inverses (2)" . property . forAllShrink arbitrary shrink $ do
            \(nubBy ((==) `on` (\(f, d, _) -> (f, d))) -> delegs) ->
                sort (toList (fromList delegs)) `shouldBe` sort delegs

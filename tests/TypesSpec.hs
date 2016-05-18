{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE OverloadedStrings    #-}

module TypesSpec where

import Data.Binary (encode, decode)
import Data.Maybe (isJust, isNothing)
import Data.Monoid ((<>))
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)
import Test.QuickCheck (property)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as Aeson

import Arbitrary ()
import Frontend.Prelude (when)
import Types


-- | run also the tests that take many seconds
beThorough :: Bool
beThorough = False

spec :: Spec
spec = do
    describe "Timestamp" $ do
        it "encode and decode are inverses" . property $
            \x -> decode (encode x) == (x :: Timestamp)
        it "read and show are inverses" . property $
            \x -> read (show x) == (x :: Timestamp)
        it "parseTimestamp and showTimestamp are inverses" . property $
            isJust . parseTimestamp . showTimestamp
        it "parseTimestamp should fail on noise" . property $
            isNothing . parseTimestamp . (<> "noise") . showTimestamp

    describe "Timespan" $ do
        it "aeson encode and decode are inverses" . property $
            \(x :: Timespan) ->
                  -- because historically, there has been a distinction between top-level values
                  -- (arrays and objects), and internal values (also literals like strings), and
                  -- because we depend on a rather old aeson version, we work on an object in this
                  -- test.
                  let x' = Aeson.object ["value" Aeson..= x]
                  in Aeson.decode (Aeson.encode x') == Just x'

    describe "diffTimestamps(-), addTimespan(+)" $ do
        it "(y+x)-x = y" . property $
            \(x :: Timestamp) (y :: Timespan) ->
                timespanUs ((y `addTimespan` x) `diffTimestamps` x) `shouldBe` timespanUs y
        it "y-((y-x)+x) = 0" . property $
            \(x :: Timestamp) (y :: Timestamp) ->
                timespanUs (y `diffTimestamps` ((y `diffTimestamps` x) `addTimespan` x)) `shouldBe` 0

    when beThorough $ do
        describe "DelegationNetwork" $ do
            it "generates" . property $
                \(dn :: DelegationNetwork) -> length (show dn) `shouldNotBe` 0
            it "aeson-encodes" . property $
                \(dn :: DelegationNetwork) -> LBS.length (Aeson.encode dn) `shouldNotBe` 0

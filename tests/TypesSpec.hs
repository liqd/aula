{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module TypesSpec where

import Arbitrary ()
import Data.Monoid ((<>))
import Types

import Data.Binary (encode, decode)
import Data.Maybe (isJust, isNothing)

import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (property)

spec :: Spec
spec = do
    timestamp

timestamp = describe "Timestamp" $ do
    it "encode and decode are inverses" . property $
        \x -> decode (encode x) == (x :: Timestamp)
    it "read and show are inverses" . property $
        \x -> read (show x) == (x :: Timestamp)
    it "parseTimestamp and renderTimestamp are inverses" . property $
        isJust . parseTimestamp . renderTimestamp
    it "parseTimestamp should fail on noise" . property $
        isNothing . parseTimestamp . (<> "noise") . renderTimestamp

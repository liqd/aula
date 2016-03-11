module TypesSpec where

import Data.Binary (encode, decode)
import Data.Maybe (isJust, isNothing)
import Data.Monoid ((<>))
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (property)

import Arbitrary ()
import Types


spec :: Spec
spec = do
    describe "Timestamp" $ do
        it "encode and decode are inverses" . property $
            \x -> decode (encode x) == (x :: Timestamp)
        it "read and show are inverses" . property $
            \x -> read (show x) == (x :: Timestamp)
        it "parseTimestamp and renderTimestamp are inverses" . property $
            isJust . parseTimestamp . renderTimestamp
        it "parseTimestamp should fail on noise" . property $
            isNothing . parseTimestamp . (<> "noise") . renderTimestamp

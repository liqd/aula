module TypesSpec where

import Arbitrary
import Types

import Data.Binary (encode, decode)

import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (property)

spec :: Spec
spec = do
    timestamp

timestamp = describe "Timestamp" $ do
    it "encode and decode are inverses" $ property $
        \x -> (decode $ encode x) == (x :: Timestamp)
    it "read and show are inverses" $ property $
        \x -> (read $ show x) == (x :: Timestamp)
    it "parseTimestamp and renderTimestamp are inverses" $ property $
        \x -> (parseTimestamp $ renderTimestamp x) == Just (x :: Timestamp)
    it "parseTimestamp should fail on noise" $ property $
        \x -> (parseTimestamp $ (++"noise") $ renderTimestamp x) == Nothing

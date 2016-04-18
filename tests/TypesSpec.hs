{-# LANGUAGE ScopedTypeVariables  #-}

module TypesSpec where

import Data.Binary (encode, decode)
import Data.Maybe (isJust, isNothing)
import Data.Monoid ((<>))
import Test.Hspec (Spec, describe, it, shouldNotBe)
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

    when beThorough $ do
        describe "DelegationNetwork" $ do
            it "generates" . property $
                \(dn :: DelegationNetwork) -> length (show dn) `shouldNotBe` 0
            it "aeson-encodes" . property $
                \(dn :: DelegationNetwork) -> LBS.length (Aeson.encode dn) `shouldNotBe` 0

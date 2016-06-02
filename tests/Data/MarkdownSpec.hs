{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE OverloadedStrings    #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Data.MarkdownSpec where

import Data.Monoid ((<>))
import Test.Hspec (Spec, describe, it, shouldBe, pending)
import Test.QuickCheck (property)

import Arbitrary ()
import Types


spec :: Spec
spec = do
    describe "Monoid" $ do
        it "nil <> x === x" . property $
            \(x :: Document) -> nil <> x `shouldBe` x
        it "x <> nil === x" . property $
            \(x :: Document) -> x <> nil `shouldBe` x
        it "x <> (y <> z) === (x <> y) <> z" . property $
            \(x :: Document) y z -> x <> (y <> z) `shouldBe` (x <> y) <> z

    describe "html" $ do
        it "rejects bad html" $ do
            markdown "<script>" `shouldBe` Left ["illegal html tag: TagOpen \"script\" []"]
        it "accepts good html" $ do
            pending
            (unMarkdown <$> markdown "<h1>") `shouldBe` Right "<h1>"


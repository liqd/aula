{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE OverloadedStrings    #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Data.MarkdownSpec where

import Data.Monoid ((<>))
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe, pending)
import Test.QuickCheck (property)

import Data.Markdown.HtmlWhiteLists as WhiteLists
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
        it "understands whitelists (html5 elems)" $ show htmlElements   `shouldNotBe` nil
        it "understands whitelists (html5 attrs)" $ show htmlAttributes `shouldNotBe` nil
        it "understands whitelists (css3)"        $ show css3Properties `shouldNotBe` nil

        it "rejects bad html elems" $ do
            markdown "<script>" `shouldBe` Left ["unsafe html element: script"]
        it "rejects bad html attrs" $ do
            pending
        it "rejects bad css props" $ do
            pending
        it "accepts good html" $ do
            let good = "<div align=\"center\" style=\"padding-top:13;\">"
            (unMarkdown <$> markdown good) `shouldBe` Right good

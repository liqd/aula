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
        it "understands whitelists (html5 elems)" $ show html5Elements   `shouldNotBe` nil
        it "understands whitelists (html5 attrs)" $ show html5Attributes `shouldNotBe` nil
        it "understands whitelists (css3)"        $ show css3Properties  `shouldNotBe` nil
  
        it "rejects bad html" $ do
            markdown "<script>" `shouldBe` Left ["unsafe html5 element: script"]
        it "accepts good html" $ do
            pending
            (unMarkdown <$> markdown "<h1>") `shouldBe` Right "<h1>"


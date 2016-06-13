{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE OverloadedStrings    #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Data.MarkdownSpec where

import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)

import Data.Markdown.HtmlWhiteLists as WhiteLists
import Arbitrary ()
import Types


spec :: Spec
spec = do
    describe "html" $ do
        it "understands whitelists (html5 elems)" $ show htmlElements   `shouldNotBe` nil
        it "understands whitelists (html5 attrs)" $ show htmlAttributes `shouldNotBe` nil
        it "understands whitelists (css3)"        $ show css3Properties `shouldNotBe` nil

        it "rejects bad html elems" $ do
            markdown "<script>" `shouldBe` Left ["unsafe html element: script"]
        it "rejects bad html attrs" $ do
            markdown "<img crossorigin=\"\">" `shouldBe` Left ["unsafe html attribute: crossorigin"]
        it "rejects bad css props" $ do
            markdown "<img style=\"orphans:13\">" `shouldBe` Left ["unsafe css property: orphans"]
        it "accepts good html" $ do
            let good = "<div align=\"center\" style=\"padding-top:13;\">"
            (unMarkdown <$> markdown good) `shouldBe` Right good

{-# OPTIONS_GHC -Werror -Wall #-}

module FrontendSpec
where

import Test.Hspec
import Frontend ()

spec :: Spec
spec = do
    describe "catchAulaExcept" $ do
        let handles500 = do
                it "writes error message to stdout" $ do
                    pending
                it "sends an apologetic 500 http response based on type `Page500`" $ do
                    pending

            handles303 = do
                it "redirects to the indicated uri." $ do
                    pending

        context "on `error`"                               $ handles500
        context "on `throw 303`"                           $ handles303
        context "on all other `throw Action.ActionExcept`" $ handles500

    describe "catch404" $ do
        context "when routing table has no matching entry" $ do
            it "writes error message to stdout" $ do
                pending
            it "sends an 404 response based on type `Page404`" $ do
                pending

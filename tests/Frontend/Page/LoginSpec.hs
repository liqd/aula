{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Page.LoginSpec
where

import AulaTests

spec :: Spec
spec = describe "logging in" $ do

  let
    checkLogin query user pass code = do
        l <- post query "/login" [partString "/login.user" user, partString "/login.pass" pass]
        (l ^. responseStatus . statusCode) `shouldBe` code

    checkLoggedIn query code = do
        l <- get query "/space"
        (l ^. responseStatus . statusCode) `shouldBe` code

  describe "with standard initial DB" . around withServer $ do
    it "redirects you if not logged in" $ \query -> do
      checkLoggedIn query 303

    context "if user does not exist" $ do
      it "will not log you in and will display something" $ \query -> do
        checkLogin query "not the admin" "foo" 500
        checkLoggedIn query 303

    context "if user does exist" $ do
      context "if password is wrong" $ do
        it "will not log you in and will display something" $ \_query -> do
          pendingWith "this prototype doesn't do this yet."

      context "if password is correct" $ do
        it "will indeed log you in (yeay)" $ \query -> do
            checkLogin query "admin" "admin" 303
            checkLoggedIn query 200

{-# LANGUAGE OverloadedStrings #-}
module Frontend.Page.LoginSpec where

import AulaTests

spec :: Spec
spec = describe "logging in" $ do
  describe "with standard initial DB" . around withServer $ do
    it "redirects you if not logged in" $ \wreq -> do
      checkLoggedIn wreq 303

    context "if user does not exist" $ do
      it "will not log you in and will redirect" $ \wreq -> do
        -- FIXME: Figure out what went wrong.
        pendingWith "Quote is in the username... Login : unerwartet &quot; &quot;erwartet 4-12 Buchstaben"
        -- FIXME is 201 the right code here, since we got some validation errors?
        checkLogin wreq "not the admin" "foobar" 201 [bodyShouldContain "Falscher Nutzername und/oder falsches Passwort."]
        checkLoggedIn wreq 303

    context "if user does exist" $ do
      context "if password is wrong" $ do
        it "will not log you in and will display something" $ \_wreq -> do
          pendingWith "this prototype doesn't do this yet."

      context "if initial password is correct" $ do
        it "will indeed log you in (yeay)" $ \wreq -> do
            checkLogin wreq "admin" "pssst" 303 []
            checkLoggedIn wreq 303

  where
    checkLogin wreq user pass code checks = do
        post wreq "/login" [partString "/login.user" user, partString "/login.pass" pass]
          `shouldRespond` (codeShouldBe code : checks)

    checkLoggedIn wreq code = get wreq "/space" `shouldRespond` [codeShouldBe code]

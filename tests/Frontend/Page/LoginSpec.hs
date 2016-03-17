{-# LANGUAGE OverloadedStrings #-}
module Frontend.Page.LoginSpec where

import AulaTests

spec :: Spec
spec = describe "logging in" $ do
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

  where
    checkLogin query user pass code = do
        post query "/login" [partString "/login.user" user, partString "/login.pass" pass]
          `shouldRespond` [codeShouldBe code]

    checkLoggedIn query code = get query "/space" `shouldRespond` [codeShouldBe code]

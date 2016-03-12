{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Page.LoginSpec
where

import Control.Concurrent (forkIO, killThread)
import Control.Exception
import Control.Lens
import Data.String.Conversions (ST, cs, (<>))
import Network.Wreq
import Test.Hspec

import Config
import Frontend


withServer :: (ST -> IO a) -> IO a
withServer action = bracket
    (forkIO $ runFrontend cfg)
    killThread
    (const $ action uri)
  where
    cfg = Config.config
    uri = "http://" <> cs (cfg ^. listenerInterface) <> ":" <> (cs . show $ cfg ^. listenerPort)

spec :: Spec
spec = describe "logging in" $ do

  describe "with standard initial DB" . around withServer $ do
    let opts :: Options
        opts = defaults

    context "if user does not exist" $ do
      it "will not log you in and will display something" $ \_uri -> do
--        l <- postWith opts (cs uri <> "/login") [partString "/login.user" "not the admin", partString "/login.pass" "foo"]
        pendingWith "this prototype doesn't do this yet."

    context "if user does exist" $ do
      context "if password is wrong" $ do
        it "will not log you in and will display something" $ \_uri -> do
          pendingWith "this prototype doesn't do this yet."

      context "if password is correct" $ do
        it "will indeed log you in (yeay)" $ \uri -> do
            l <- postWith opts (cs uri <> "/login") [partString "/login.user" "admin", partString "/login.pass" "admin"]
            (l ^. responseStatus . statusCode) `shouldBe` 200

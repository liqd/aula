{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE OverloadedStrings    #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module SeleniumSpec where

import System.Timeout
import Test.Hspec
import Test.WebDriver

import Arbitrary ()
import AulaTests


wdConfig :: WDConfig
wdConfig = useBrowser (chrome { chromeBinary = Just "/usr/bin/chromium-browser", chromeOptions = ["--no-sandbox"] }) defaultConfig

-- | in ms
globalTimeout :: Num n => n
globalTimeout = 55300


spec :: Spec
spec = do
    describe "@Selenium" . around withServer $ do
        it "works" $ \wreq -> do
            title <- liftIO . timeout (1000 * globalTimeout) . runSession wdConfig $ do
                openPage (site wreq)
                getText =<< findElem (ByTag "h1")
            title `shouldBe` Just "Willkommen bei Aula"

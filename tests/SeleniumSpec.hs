{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE ViewPatterns         #-}

{-# OPTIONS_GHC -Werror -Wall -fno-warn-incomplete-patterns #-}

-- | selenium tests.
--
-- run these by running `make selenium` in a terminal in the aula-docker image.
--
-- for debugging, you have two options:
--
-- 1. sprinkle @getSource >>= writeFile "/page.html"@, @saveScreenshot "/screenshot.png"@
--    over your 'WD' monads.
-- 2. watch with vncviewer (see `/docs/testing.md`).
module SeleniumSpec where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Text as ST
import System.Timeout
import Test.Hspec
import Test.WebDriver
import Test.WebDriver.Missing

import Arbitrary ()
import AulaTests


wdConfig :: WDConfig
wdConfig = useChrome defaultConfig
  where
    useChrome = useBrowser (chrome { chromeBinary = Just "/usr/bin/chromium-browser"
                                   , chromeOptions = ["--no-sandbox"]
                                   })

runWDAula :: (MonadIO m) => WD a -> m (Maybe a)
runWDAula = liftIO . timeout (1000 * globalTimeout) . runSession wdConfig . finallyClose

-- | in ms
globalTimeout :: Num n => n
globalTimeout = 10300


spec :: Spec
spec = do
    describe "@Selenium" . around withServer $ do
        it "works" $ \wreq ->
            runWDAula (openPage (mkUri wreq "") >> findElem (ByTag "h1") >>= getText)
              `shouldReturn` Just "Willkommen bei Aula"

        it "proofs a concept" $ \wreq -> do
            imgdata <- runWDAula $ do
                -- login and visit own profile
                openPage (mkUri wreq "")
                sendKeys "admin" =<< findElem (ByXPath "//input[@id='/login.user']")
                sendKeys "pssst" =<< findElem (ByXPath "//input[@id='/login.pass']")
                submit           =<< findElem (ByXPath ".//input[@type='submit']")
                click            =<< findElem (ByXPath ".//span[@class='user-name']")
                jsGetBase64Image "//img"

            -- liftIO $ print imgdata
            -- Just (Object (fromList [("value",String "data:image/png;base64,[...]")]))

            case imgdata of
                Just (Object (HM.toList -> [("value", String value)])) ->
                    ST.unpack value `shouldContain` "data:image/png;base64,"

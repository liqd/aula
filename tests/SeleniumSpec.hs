{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

{-# OPTIONS_GHC -Werror -Wall #-}

-- | selenium tests.
--
-- run these by running `make selenium` in a terminal in the aula-docker image.
--
-- for debugging, you have two options:
--
-- 1. sprinkle @getSource >>= writeFile "/page.html"@, @saveScreenshot "/screenshot.png"@
--    over your 'WD' monads.
-- 2. run find out the IP address of your docker image (run `ifconfig` in a terminal), and
--    run `vncviewer 172.17.0.5:5900` from your host.
module SeleniumSpec where

import System.Timeout
import Test.Hspec
import Test.WebDriver

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

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

-- {-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Page.FileUploadSpec
where

import Control.Concurrent (forkIO, killThread)
import Control.Exception
import Control.Lens
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.String.Conversions (ST, cs, (<>))
import Lucid (renderText, toHtml)
import Network.Wai
import Servant
import Test.Hspec  -- (Spec, describe, context, it)
import Network.Wreq

import Action
import Frontend
import Frontend.Core
import Api.Persistent
import Config


withServer :: (ST -> IO a) -> IO a
withServer action = bracket
    (forkIO $ runFrontend cfg)
    killThread
    (const $ action uri)
  where
    cfg = Config.config
    uri = "http://" <> cs (cfg ^. listenerInterface) <> ":" <> (cs . show $ cfg ^. listenerPort)
    -- uri = "http://localhost:8081"

spec :: Spec
spec = describe "file upload" . around withServer $ do
    let classPart :: Part
        classPart = partString "/testing/file-upload.classname" "7a"

        filePart :: Part
        filePart = (partFileName .~ Just "x.csv") . (partContentType .~ Just "text/csv") $ p
          where
            p = partString "/testing/file-upload.file" $ unlines
                    [ "Vorname;Nachname;email;Login-Name"
                    , "Hein;Blöd;bloed@example.org"
                    , "Heidi;Schmumel;br@example.org;mup"
                    , "Jens;Kuhn;jens@example.org"
                    ]

        opts :: Options
        opts = defaults

    it "works" $ \uri -> do
        pendingWith "only partially implemented."

        -- the response will be a 500 saying "SUCCESS!" and mirroring the parsed csv file back into
        -- the browser.  what we actually want to test is that the response is a 303 and that the
        -- database will contain the data we posted.

        r <- postWith opts (cs uri <> "/testing/file-upload") [classPart, filePart]
        liftIO $ print r

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Page.FileUploadSpec
where

import Control.Concurrent (forkIO, killThread)
import Control.Exception
import Control.Lens
import Control.Monad (forM_)
import Data.String.Conversions (ST, LBS, cs, (<>))
import Network.Wreq
import Test.Hspec (Spec, describe, it, around, shouldBe, shouldContain)

import qualified Data.ByteString.Lazy.Char8 as LBS

import Action
import Config
import Frontend
import Frontend.Page.FileUpload


withServer :: (ST -> IO a) -> IO a
withServer action = bracket
    (forkIO $ runFrontend cfg)
    killThread
    (const $ action uri)
  where
    cfg = Config.test
    uri = "http://" <> cs (cfg ^. listenerInterface) <> ":" <> (cs . show $ cfg ^. listenerPort)
    -- uri = "http://localhost:8081"

spec :: Spec
spec = describe "file upload" $ do
    describe "http" . around withServer $ do
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

        it "posts users successfully; users will appear under /user" $ \uri -> do
            -- pendingWith "only partially implemented."
            l <- postWith opts (cs uri <> "/login") [partString "/login.user" "admin", partString "/login.pass" "adminPass"]
            (l ^. responseStatus . statusCode) `shouldBe` 200
            r <- postWith opts (cs uri <> "/testing/file-upload") [classPart, filePart]
            (r ^. responseStatus . statusCode) `shouldBe` 200
            s <- get (cs uri <> "/user")
            (s ^. responseStatus . statusCode) `shouldBe` 200
            (cs $ s ^. responseBody :: String) `shouldContain` "_fromUserLastName = &quot;Kuhn&quot"

    describe "csv file parser" $ do
        let ts :: [(String, [LBS])]
            ts = [ ("empty file",
                    "Vorname;Nachname;email;Login-Name" :
                    [])
                 , ("first and last name only",
                    "Vorname;Nachname;email;Login-Name" :
                    "Hein;Blöd" :
                    [])
                 , ("names and email",
                    "Vorname;Nachname;email;Login-Name" :
                    "Hein;Blöd;bloed@example.org" :
                    [])
                 , ("names, email, nick",
                    "Vorname;Nachname;email;Login-Name" :
                    "Hein;Blöd;bloed@example.org;nick" :
                    [])
                 , ("names and nick",
                    "Vorname;Nachname;email;Login-Name" :
                    "Hein;Blöd;;nick" :
                    [])
                 , ("multiple records",
                    "Vorname;Nachname;email;Login-Name" :
                    "Hein;Blöd;bloed@example.org" :
                    "Heidi;Schmumel;br@example.org;mup" :
                    "Jens;Kuhn;jens@example.org" :
                    "Jens2;Kuhn2;;ock" :
                    [])
                 ]

        forM_ ts $ \(label, file) -> it label $ do
            let v :: Either String [CsvUserRecord] = decodeCsv $ LBS.unlines file
            length <$> v `shouldBe` Right (length file - 1)

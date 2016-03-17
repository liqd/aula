{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Page.FileUploadSpec
where

import Control.Concurrent (forkIO, killThread)
import Control.Exception
import Control.Lens
import Control.Monad (forM_)
import Data.String.Conversions (LBS, cs, (<>))
import Network.Wreq hiding (get, post)
import Network.Wreq.Types (Postable, StatusChecker)
import Test.Hspec (Spec, describe, it, around, shouldBe, shouldContain)

import qualified Network.Wreq.Session as Sess
import qualified Data.ByteString.Lazy.Char8 as LBS

import Action
import Config
import Frontend
import Frontend.Page.FileUpload

-- Same as Frontend.Page.LoginSpec.Query
data Query = Query
    { post :: forall a. Postable a => String -> a -> IO (Response LBS)
    , get  :: String -> IO (Response LBS)
    }

-- Same as Frontend.Page.LoginSpec.doNotThrowExceptionsOnErrorCodes
doNotThrowExceptionsOnErrorCodes :: StatusChecker
doNotThrowExceptionsOnErrorCodes _ _ _ = Nothing

-- Same as Frontend.Page.LoginSpec.withServer
withServer :: (Query -> IO a) -> IO a
withServer action = bracket
    (forkIO $ runFrontend cfg)
    killThread
    (const . Sess.withSession $ action . query)
  where
    cfg = Config.test
    uri path = "http://" <> cs (cfg ^. listenerInterface) <> ":" <> (cs . show $ cfg ^. listenerPort) <> path
    opts = defaults & checkStatus .~ Just doNotThrowExceptionsOnErrorCodes
                    & redirects   .~ 0
    query sess = Query (Sess.postWith opts sess . uri) (Sess.getWith opts sess . uri)

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

        it "posts users successfully; users will appear under /user" $ \query -> do
            -- pendingWith "only partially implemented."
            l <- post query "/login" [partString "/login.user" "admin", partString "/login.pass" "adminPass"]
            (l ^. responseStatus . statusCode) `shouldBe` 303
            r <- post query "/testing/file-upload" [classPart, filePart]
            (r ^. responseStatus . statusCode) `shouldBe` 303
            s <- get query "/user"
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

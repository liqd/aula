{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Page.FileUploadSpec
where

import qualified Data.ByteString.Lazy.Char8 as LBS

import AulaTests
import Frontend.Page.Admin
import qualified Frontend.Path as U


fileUploadPath :: (IsString s) => String -> s
fileUploadPath rest =
    fromString $ (cs . absoluteUriPath . relPath . U.Admin $ U.AdminAccess PermClassCreate) <> rest

spec :: Spec
spec = describe "file upload" $ do
    describe "http" . around withServer $ do
        let classPart :: Part
            classPart = partString (fileUploadPath ".classname") "7a"

            filePart :: Part
            filePart = p & partFileName ?~ "x.csv"
                         & partContentType ?~ "text/csv"
              where
                p = partString (fileUploadPath ".file") $ unlines
                        [ "Vorname;Nachname;email;Login-Name"
                        , "Hein;Blöd;bloed@example.org"
                        , "Heidi;Schmumel;br@example.org;mup"
                        , "Jens;Kuhn;jens@example.org"
                        ]

        it "posts users successfully; users will appear under /user" $ \query -> do
            post query "/login"
                [partString "/login.user" "admin", partString "/login.pass" "adminPass"]
                `shouldRespond` [codeShouldBe 303]
            post query (fileUploadPath "") [classPart, filePart]
                `shouldRespond` [codeShouldBe 303]
            get query "/user"
                `shouldRespond` [codeShouldBe 200
                                ,bodyShouldContain "_fromUserLastName = &quot;Kuhn&quot"]

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

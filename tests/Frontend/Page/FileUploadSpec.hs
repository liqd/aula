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
    fromString $ (cs . absoluteUriPath $ U.relPath U.adminCreateClass) <> rest

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

        it "posts users successfully; users will appear under /testing/users" $ \wreq -> do
            loginAsAdmin wreq
            t <- get wreq "/testing/csrf_token"
            let tokenPart = partString (fileUploadPath "._csrf") (t ^. responseBody . csi)
            post wreq (fileUploadPath "") [tokenPart, classPart, filePart]
                `shouldRespond` [codeShouldBe 303]
            get wreq "/testing/users"
                `shouldRespond` [codeShouldBe 200
                                ,bodyShouldContain "_unUserLastName = &quot;Kuhn&quot"]

    describe "csv file parser" $ do
        -- valid input
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
            let v :: Either String [CsvUserRecord] = catMaybes <$> decodeCsv (LBS.unlines file)
            length <$> v `shouldBe` Right (length file - 1)

        -- input sanitizing
        let is :: [(String, [LBS], [(ST, ST)])]
            is = [ ("UTF-8 with LF linebreaks",
                    "Vorname;Nachname" :
                    "Jühti Toih;Mtohi" :
                    "Mhyi;Syti" :
                    "Muria;Tüßq" :
                    [],
                    ("Jühti Toih", "Mtohi") :
                    ("Mhyi", "Syti") :
                    ("Muria", "Tüßq") :
                    [])
                 , ("ISO-8859 with CRLF linebreaks",
                    "Vorname;Nachname\r" :
                    "J\252hti Toih;Mtohi\r" :
                    "Mhyi;Syti\r" :
                    "Muria;T\246\223q\r" :
                    [],
                    ("Jühti Toih", "Mtohi") :
                    ("Mhyi", "Syti") :
                    ("Muria", "Tößq") :
                    [])
                 , ("Empty names",
                    "Vorname;Nachname\r" :
                    "Wrg;Oiuy\r" :
                    ";\r" :
                    "" :
                    "" :
                    ";\r" :
                    "" :
                    "Mhyi;Syti\r" :
                    ";\r" :
                    "" :
                    ";\r" :
                    "" :
                    [],
                    ("Wrg", "Oiuy") :
                    ("Mhyi", "Syti") :
                    [])
                 ]

        forM_ is $ \(label, file, wantedNames) -> it label $ do
            let Right v :: Either String [CsvUserRecord] = catMaybes <$> decodeCsv (LBS.unlines file)
            forM_ (zip v wantedNames) $
                \( CsvUserRecord (UserFirstName gotFirstName) (UserLastName gotLastName)
                                              Nothing Nothing Nothing
                 , (wantedFirstName, wantedLastName)
                 ) -> do
                       print (gotFirstName, gotLastName)
                       gotFirstName `shouldBe` wantedFirstName
                       gotLastName  `shouldBe` wantedLastName

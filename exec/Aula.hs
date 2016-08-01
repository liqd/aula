{-# LANGUAGE ScopedTypeVariables  #-}

module Main where

import Data.Time
import System.Directory
import System.IO
import Text.Show.Pretty

import Frontend
import Config
import Action.Smtp


main :: IO ()
main = do
    setCurrentDirectoryToAulaRoot
    -- FIXME: Do not use print.
    cfg <- readConfig print CrashMissing
    now <- getCurrentTime
    checkSendMail cfg
    checkAvatarPathExists cfg

    wd <- getCurrentDirectory
    hPutStrLn stderr $ unlines
        [ ""
        , show now
        , "this is aula-server!"
        , "\nrelease:"
        , Config.releaseVersion
        , "\nroot path:"
        , wd
        , "\nsetup:", ppShow cfg
        , ""
        ]

    runFrontend cfg

{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Main where

import Data.Time
import System.Directory
import Text.Show.Pretty

import qualified Data.Text as ST

import Action.Smtp
import AulaPrelude
import Config
import Daemon
import Frontend
import Logger
import Types


main :: IO ()
main = do
    setCurrentDirectoryToAulaRoot
    cfg <- readConfig CrashMissing
    now <- getCurrentTime
    checkSendMail cfg
    checkAvatarPathExists cfg

    wd <- getCurrentDirectory
    aulaLog (cfg ^. logging) . LogEntry INFO . ST.unlines $
        [ ""
        , cshow now
        , "this is aula-server!"
        , "\nrelease:"
        , cs Config.releaseVersion
        , "\nroot path:"
        , cs wd
        , "\nsetup:", cs $ ppShow cfg
        , ""
        ]

    runFrontend cfg

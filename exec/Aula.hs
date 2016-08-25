{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Main where

import System.Directory

import Action.Smtp
import Config
import Frontend


main :: IO ()
main = do
    setCurrentDirectoryToAulaRoot
    cfg <- readConfig CrashMissing
    checkSendMail cfg
    checkAvatarPathExists cfg

    wd <- getCurrentDirectory
    logmotd cfg wd

    runFrontend cfg

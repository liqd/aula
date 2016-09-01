{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Main where

import Data.Monoid ((<>))
import Control.Exception
import System.Directory

import Action.Smtp
import Config
import Frontend


main :: IO ()
main = do
    setCurrentDirectoryToAulaRoot
    cfg <- readConfig CrashMissing
    checkSendMail cfg
    checkAvatarPathExists cfg `catch` (\(ErrorCall msg) -> throwIO . ErrorCall $
        msg <> "\n\ndid you run aula-init-state?\n\n")

    wd <- getCurrentDirectory
    logmotd cfg wd

    runFrontend cfg

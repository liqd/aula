{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Main where

import Control.Exception
import Control.Lens ((^.))
import Data.String.Conversions (cs, (<>))
import System.Directory

import Action.Smtp
import Config
import Frontend
import Logger
import Types.Log


main :: IO ()
main = do
    setCurrentDirectoryToAulaRoot
    cfg <- readConfig CrashMissing
    checkSendMail cfg
    checkAvatarPathExists cfg `catch` (\(ErrorCall msg) -> throwIO . ErrorCall $
        msg <> "\n\ndid you run aula-init-state?\n\n")

    wd <- getCurrentDirectory
    logmotd cfg wd

    runFrontend cfg `catch` \(e :: SomeException) -> do
        let lg = unSendLogMsg (aulaLog (cfg ^. logging)) . LogEntry INFO . cs
        lg (show e)
        lg "shutdown."

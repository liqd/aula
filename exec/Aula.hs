{-# LANGUAGE ScopedTypeVariables  #-}

module Main where

import System.Directory
import System.IO
import Text.Show.Pretty

import Frontend
import Config


main :: IO ()
main = do
    setCurrentDirectoryToAulaRoot
    cfg <- getConfig CrashMissing

    wd <- getCurrentDirectory
    hPutStrLn stderr $ unlines
        [ ""
        , "this is aula!"
        , "\nrelease:"
        , Config.releaseVersion
        , "\nroot path:"
        , wd
        , "\nsetup:", ppShow cfg
        , ""
        ]

    runFrontend cfg

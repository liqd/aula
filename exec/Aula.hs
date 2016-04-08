{-# LANGUAGE ScopedTypeVariables  #-}

module Main where

import Data.Monoid ((<>))

import Frontend
import Config

main :: IO ()
main = do
    setCurrentDirectoryToAulaRoot
    cfg <- readConfig CrashMissing
    logger cfg $ "running aula with config " <> show cfg
    runFrontend cfg

{-# LANGUAGE ScopedTypeVariables  #-}

module Main where

import Types
import Frontend
import Config

main :: IO ()
main = do
    setCurrentDirectoryToAulaRoot
    runFrontend

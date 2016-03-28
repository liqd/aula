{-# LANGUAGE ScopedTypeVariables  #-}

module Main where

import Data.Monoid ((<>))

import Frontend
import Config

main :: IO ()
main = do
    setCurrentDirectoryToAulaRoot
    cfg <- getConfig WarnMissing
    putStrLn $ "running aula with config " <> show cfg
    runFrontend cfg

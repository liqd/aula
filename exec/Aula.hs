{-# LANGUAGE ScopedTypeVariables  #-}

module Main where

import Data.Monoid ((<>))

import Frontend
import Config

main :: IO ()
main = do
    setCurrentDirectoryToAulaRoot
    let cfg = Config.devel
    putStrLn $ "running aula with config " <> show cfg
    runFrontend cfg

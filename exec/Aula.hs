{-# LANGUAGE ScopedTypeVariables  #-}

module Main where

import Data.Monoid ((<>))

import Frontend
import Config

main :: IO ()
main = do
    setCurrentDirectoryToAulaRoot
    putStrLn $ "running aula with config " <> show Config.config
    runFrontend

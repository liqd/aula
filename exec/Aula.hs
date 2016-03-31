{-# LANGUAGE ScopedTypeVariables  #-}

module Main where

import Data.Monoid ((<>))

import Frontend
import Config

main :: IO ()
main = do
    setCurrentDirectoryToAulaRoot
    cfg <- getConfig WarnMissing
    putStrLn $ "running aula with config " <> show cfg  -- FIXME use logger for this
    runFrontend cfg

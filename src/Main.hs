{-# LANGUAGE ScopedTypeVariables  #-}

module Main
where

import Control.Lens
import Data.Acid

import Frontend
import Transaction
import Types
import System.Environment

import qualified Config


main :: IO ()
main = do
    database :: AcidState Db <- openLocalStateFrom (Config.config ^. Config.dbPath) mempty
    return ()

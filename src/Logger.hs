{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Logger
where

import Data.String.Conversions (ST)
import Data.Yaml
import GHC.Generics

data LogLevel
    = DEBUG
    | INFO
    | WARN
    | ERROR
    | NOLOG
  deriving (Eq, Generic, Ord, Show, FromJSON, ToJSON)

data LogEntry = LogEntry LogLevel ST
  deriving (Eq, Show)

type SendLogMsg = LogEntry -> IO ()

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Logger
where

import Data.String.Conversions (ST)
import Data.Yaml
import GHC.Generics

import Logger.EventLog


data LogLevel
    = DEBUG
    | INFO
    | WARN
    | ERROR
    | NOLOG
  deriving (Eq, Generic, Ord, Show, FromJSON, ToJSON)

-- FIXME: EventLog should be merged with this module and everything in it renamed to something
-- saying `moderator`; ST should change into an ADT that has a constructor for EventLogItems, but
-- also other constructors for things like `disk full`.  The logger can then have several targets
-- (moderator event log, syslog, admin email, devops email, ...), and decide what to do with each
-- event based on its type.
data LogEntry =
    LogEntry LogLevel ST
  | LogEntryForModerator EventLogItemCold
  deriving (Eq, Show)

type SendLogMsg = LogEntry -> IO ()

nullLog :: LogEntry -> IO ()
nullLog _ = pure ()

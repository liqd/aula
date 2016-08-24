{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Logger
where

import Control.Lens (makeLenses)
import Control.Monad (when)
import Data.Aeson
import Data.Monoid ((<>))
import Data.String.Conversions (ST, cs)
import Data.Time (getCurrentTime)
import GHC.Generics
import System.IO (stderr)

import qualified Data.Text.IO as ST
import qualified Data.ByteString.Lazy as LBS

import Logger.EventLog
import Types.Log (LogLevel(NOLOG))
import Types.Prelude (cshow)


-- FIXME: EventLog should be merged with this module and everything in it renamed to something
-- saying `moderator`; ST should change into an ADT that has a constructor for EventLogItems, but
-- also other constructors for things like `disk full`.  The logger can then have several targets
-- (moderator event log, syslog, admin email, devops email, ...), and decide what to do with each
-- event based on its type.
data LogEntry =
    LogEntry LogLevel ST
  | LogEntryForModerator EventLogItemCold
  deriving (Eq, Show)

data LogConfig = LogConfig
    { _logCfgLevel  :: LogLevel
    , _logCfgPath   :: FilePath
    , _eventLogPath :: FilePath
    }
  deriving (Show, Generic, ToJSON, FromJSON)

makeLenses ''LogConfig

newtype SendLogMsg = SendLogMsg { unSendLogMsg :: LogEntry -> IO () }


nullLog :: SendLogMsg
nullLog = SendLogMsg $ \_ -> pure ()

stderrLog :: SendLogMsg
stderrLog = SendLogMsg $ ST.hPutStrLn stderr . cshow

aulaLog :: LogConfig -> SendLogMsg
aulaLog cfg = SendLogMsg $ \case
    (LogEntry NOLOG _) ->
        pure ()
    (LogEntry level msg) ->
        when (level >= _logCfgLevel cfg) $ do
            now <- getCurrentTime
            appendFile (_logCfgPath cfg) $ cshow now <> " [" <> cshow level <> "] " <> cs msg <> "\n"
    (LogEntryForModerator ev) ->
        LBS.appendFile (_eventLogPath cfg) $ encode ev <> cs "\n"

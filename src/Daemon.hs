{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Daemon
    ( SystemLogger
    , MsgDaemon
    , TimeoutDaemon
    , Daemon(..)
    , msgDaemon
    , msgDaemonSend
    , timeoutDaemon
    , timeoutDaemon'
    , logDaemon
    )
where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception hiding (handle)
import Control.Lens
import Control.Monad (forever, join, when)
import Data.Time.Clock (getCurrentTime)
import Data.String.Conversions (cs, (<>))
import System.IO (hPutStrLn, stderr)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS

import Logger
import Types
import Config


type SystemLogger = LogEntry -> IO ()

-- | The daemon is implemented as a thread.  `_msgDaemonStart` starts a new daemon.
--
-- All implementations of `_msgDaemonStart` can be called several times.  The daemon threads thus
-- created share one channel and will race each other for the messages.  Use this to start up
-- concurrent threads if message handling takes a long time, and would otherwise block the channel.
data MsgDaemon a = MsgDaemon
    { _msgDaemonStart :: IO ThreadId
    , _msgDaemonSend  :: a -> IO ()
    }

data TimeoutDaemon = TimeoutDaemon
    { _timeoutDaemonStart :: IO ThreadId }

makeLenses ''MsgDaemon
makeLenses ''TimeoutDaemon

class Daemon d where
    start :: Getter d (IO ThreadId)

instance Daemon (MsgDaemon a) where
    start = msgDaemonStart

instance Daemon TimeoutDaemon where
    start = timeoutDaemonStart

-- | Message daemons receive typed messages over a 'Chan'.  Two example applications are logger
-- daemon (receive log messages and append them to a log file) and sendmail daemon (receive typed
-- emails over 'Chan' and deliver them).
msgDaemon
    :: SystemLogger
    -> String
    -> (a -> IO ())
    -> (SomeException -> IO ())
    -> IO (MsgDaemon a)
msgDaemon logger name computation handleException = do
    chan <- newTChanIO

    let sendMsg = atomically . writeTChan chan
        loop = forkIO . forever $ run `catch` handle
          where
            run = join . atomically $ computation <$> readTChan chan

            handle e@(SomeException e') = do
                logger . LogEntry ERROR . cs $ concat ["daemon [", name, "] ", show e']
                handleException e

    return $ MsgDaemon loop sendMsg

-- | Run an action in constant intervals (the first time *after* the first interval).
-- Example uses are phase timeout and acid-state snapshot.
timeoutDaemon
    :: SystemLogger
    -> String
    -> Timespan
    -> IO ()
    -> (SomeException -> IO ())
    -> TimeoutDaemon
timeoutDaemon logger name delay computation handleException = TimeoutDaemon $ do
    let run = do
            logger . LogEntry INFO . cs $
                concat ["daemon [", name, "] triggered (at frequency ", showTimespan delay, ")."]
            computation `catch` handle

        handle e@(SomeException e') = do
            logger . LogEntry ERROR . cs $
                concat ["daemon [", name, "] ", show e']
            handleException e

    forkIO . forever $ do
        threadDelay (timespanUs delay)
        run `catch` (\(e@(SomeException _)) -> do
            -- (alternatively, we could change the 'SystemLogger' type to a newtype, make it
            -- abstract, and make sure that every system logger we ever encounter in the wild will
            -- have a handler wrapped around it.)
            hPutStrLn stderr $ "*** timeoutDaemon: exception in except handler: " <> show e
            hPutStrLn stderr "*** timeoutDaemon: this is not good.  trying to keep running.")

-- | Same as timeoutDaemon but without any extra exception handling.
-- Errors are still sent to the logger.
timeoutDaemon'
    :: SystemLogger
    -> String
    -> Timespan
    -> IO ()
    -> TimeoutDaemon
timeoutDaemon' logger name delay computation =
    timeoutDaemon logger name delay computation (const $ pure ())


-- * Log Daemon

-- | Create a log daemon
logDaemon :: LogConfig -> IO (MsgDaemon LogEntry)
logDaemon cfg =
    msgDaemon logMsg "logger" logMsg (const $ pure ())
  where
    logMsg (LogEntry NOLOG _)        = pure ()
    logMsg (LogEntry level msg)      = when (level >= cfg ^. logLevel) $ do
                                            now <- getCurrentTime
                                            hPutStrLn stderr (cshow now <> " [" <> cshow level <> "] " <> cs msg)
    logMsg (LogEntryForModerator ev) = LBS.appendFile (cfg ^. eventLogPath) $ Aeson.encode ev <> cs "\n"

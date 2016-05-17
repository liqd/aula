{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Daemon
    ( SystemLogger
    , MsgDaemon
    , TimeoutDeamon
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
-- All implementations of `_msgDaemonStart` can be called several times.  The deamon threads thus
-- created share one channel and will race each other for the messages.  Use this to start up
-- concurrent threads if message handling takes a long time, and would otherwise block the channel.
data MsgDaemon a = MsgDaemon
    { _msgDaemonStart :: IO ThreadId
    , _msgDaemonSend  :: a -> IO ()
    }

data TimeoutDeamon = TimeoutDeamon
    { _timeoutDaemonStart :: IO ThreadId }

makeLenses ''MsgDaemon
makeLenses ''TimeoutDeamon

class Daemon d where
    start :: Getter d (IO ThreadId)

instance Daemon (MsgDaemon a) where
    start = msgDaemonStart

instance Daemon TimeoutDeamon where
    start = timeoutDaemonStart

-- | Message deamons receive typed messages over a 'Chan'.  Two example applications are logger
-- deamon (receive log messages and append them to a log file) and sendmail deamon (receive typed
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
            run = join . atomically $ do
                x <- readTChan chan
                return $ do
                    logger . LogEntry DEBUG . cs $ concat ["daemon [", name, "] recieved a message."]
                    computation x

            handle e@(SomeException e') = do
                logger . LogEntry ERROR . cs $ concat ["error occured in daemon [", name, "] ", show e']
                handleException e

    return $ MsgDaemon loop sendMsg

-- | Run an action in constant intervals.  Example uses are phase timeout and acid-state snapshot.
timeoutDaemon
    :: SystemLogger
    -> String
    -> Timespan
    -> IO ()
    -> (SomeException -> IO ())
    -> TimeoutDeamon
timeoutDaemon logger name delay computation handleException = TimeoutDeamon $ do
    let run = do
            logger . LogEntry INFO . cs $
                concat ["daemon [", name, "] timed out after ", showTimespan delay, "."]
            computation

        handle e@(SomeException e') = do
            logger . LogEntry ERROR . cs $
                concat ["error occured in daemon [", name, "] ", show e']
            handleException e

    forkIO . forever $ do
        run `catch` handle
        threadDelay (timespanUs delay)

-- | Same as timeoutDaemon' but sends error to the logger.
timeoutDaemon'
    :: SystemLogger
    -> String
    -> Timespan
    -> IO ()
    -> TimeoutDeamon
timeoutDaemon' logger name delay computation =
    timeoutDaemon logger name delay computation handleException
  where
    handleException (SomeException e) =
        logger . LogEntry ERROR . cs $
            "error when running `" <> name <> "`: " <> show e


-- * Log Daemon

-- | Create a log deamon
logDaemon :: LogConfig -> IO (MsgDaemon LogEntry)
logDaemon cfg =
    msgDaemon logMsg "logger" logMsg (const $ pure ())
  where
    logMsg (LogEntry NOLOG _)        = pure ()
    logMsg (LogEntry level msg)      = when (level >= cfg ^. logLevel) $ hPutStrLn stderr (cs msg)
    logMsg (LogEntryForModerator ev) = LBS.appendFile (cfg ^. eventLogPath) $ Aeson.encode ev <> cs "\n"

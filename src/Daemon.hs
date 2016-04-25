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
    , logDaemon
    )
where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception hiding (handle)
import Control.Lens
import Control.Monad (forever, join, when)
import Data.String.Conversions (cs)
import System.IO (hPutStrLn, stderr)

import Logger
import Types


type SystemLogger = LogEntry -> IO ()

-- | The daemon is implemented as a thread. With the _start
-- one can start a new daemon of a given kind.
--
-- For the message all the daemon instances started with the
-- same method will listen on the same message channel.

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
        threadDelay (timespanMs delay)


-- * Log Daemon

-- | Create a log deamon
logDaemon :: LogLevel -> IO (MsgDaemon LogEntry)
logDaemon minLevel =
    msgDaemon logMsg "logger" logMsg (const $ pure ())
  where
    -- FIXME: Use event logging
    logMsg (LogEntry NOLOG _) = pure ()
    logMsg (LogEntry level msg) =
        when (level >= minLevel) $ hPutStrLn stderr (cs msg)

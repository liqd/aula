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

msgDaemon
    :: SystemLogger
    -> String
    -> (a -> IO ())
    -> (SomeException -> IO ())
    -> IO (MsgDaemon a)
msgDaemon logger name computation handleException = do
    chan <- newTChanIO

    let sendMsg = atomically . writeTChan chan

    let run = join . atomically $ do
                    x <- readTChan chan
                    return $ do
                        logger . LogEntry INFO . cs $ concat ["daemon [", name, "] recieved a message."]
                        computation x

    let handle e@(SomeException e') = do
                    logger . LogEntry ERROR . cs $ concat ["error occured in daemon [", name, "] ", show e']
                    handleException e

    let loop = forkIO . forever $ run `catch` handle

    return $ MsgDaemon loop sendMsg

timeoutDaemon
    :: SystemLogger
    -> String
    -> Int
    -> IO ()
    -> (SomeException -> IO ())
    -> TimeoutDeamon
timeoutDaemon logger name delay_us computation handleException = TimeoutDeamon $ do

    let run = do
            logger . LogEntry INFO . cs $ concat ["daemon [", name, "] timed out after ", show delay_us, " us."]
            computation

    let handle e@(SomeException e') = do
            logger . LogEntry ERROR . cs $ concat ["error occured in daemon [", name, "] ", show e']
            handleException e

    forkIO . forever $ do
        run `catch` handle
        threadDelay delay_us


-- * Log Daemon

-- | Create a log deamon
logDaemon :: LogLevel -> IO (MsgDaemon LogEntry)
logDaemon minLevel =
    msgDaemon logMsg "logger" logMsg (const $ pure ())
  where
    -- FIXME: Use event logging
    logMsg (LogEntry level msg) =
        when (level >= minLevel) $ hPutStrLn stderr (cs msg)

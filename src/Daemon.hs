{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Daemon
where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception hiding (handle)
import Control.Lens
import Control.Monad (forever, join)
import Data.String.Conversions (ST, cs)
import System.IO (hPutStrLn, stderr)

type SystemLogger = String -> IO ()

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
                        logger $ concat ["daemon [", name, "] recieved a message."]
                        computation x

    let handle e@(SomeException e') = do
                    logger $ concat ["error occured in daemon [", name, "] ", show e']
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
            logger $ concat ["daemon [", name, "] timed out after ", show delay_us, " us."]
            computation

    let handle e@(SomeException e') = do
            logger $ concat ["error occured in daemon [", name, "] ", show e']
            handleException e

    forkIO . forever $ do
        run `catch` handle
        threadDelay delay_us

type SendLogMsg = ST -> IO ()

-- | Create a log deamon
logDaemon :: SystemLogger -> IO (MsgDaemon ST)
logDaemon systemLog = msgDaemon systemLog "logger" logMsg (const $ pure ())
  where
    -- FIXME: Use event logging
    logMsg = hPutStrLn stderr . cs

makeLenses ''MsgDaemon
makeLenses ''TimeoutDeamon

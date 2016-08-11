{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

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
    , unsafeLogDaemon
    , cleanUpDaemon
    )
where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception hiding (handle)
import Control.Lens
import Control.Monad (filterM, forever, forM_, join, when)
import Data.Functor.Infix ((<$$>))
import Data.List (isPrefixOf)
import Data.Time.Clock (getCurrentTime)
import Data.String.Conversions (cs, (<>))
import System.Directory
import System.FilePath
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)

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
    -> Bool
    -> IO (MsgDaemon a)
msgDaemon logger name computation handleException threadSafe = do
    chan <- newTChanIO
    alreadyRunningRef :: MVar (Maybe ThreadId) <- newMVar Nothing

    let sendMsg = atomically . writeTChan chan
        loop = do
            alreadyRunning <- takeMVar alreadyRunningRef
            case (threadSafe, alreadyRunning) of
                (False, Just tid) -> do
                    logger . LogEntry WARN . cs $
                        "no fork of thread-unsafe msgDaemon (using original thread): " <> name
                    return tid
                (False, Nothing) -> do
                    tid <- new
                    putMVar alreadyRunningRef (Just tid)
                    return tid
                _ -> do
                    new
          where
            new = forkIO . forever $ run `catch` handle
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

-- | Create a log daemon.  This is both responsible for the moderator's event log (file location
-- given in Config.hs) and system log messages (stderr).
--
-- NOTE: 'logDaemon' can only be called once, and only have one thread.  Multiple calls to 'start'
-- will return the same 'ThreadId', multiple calls to 'logDaemon' will crash.  (What would go wrong?
-- If too many `logDaemon`s are running concurrently, they might undo each others moderator event
-- log entries, or enter an inconsistent state.  also, stderr could be garbled if two lines are
-- printed concurrently.)
logDaemon :: LogConfig -> IO (MsgDaemon LogEntry)
logDaemon cfg = do
    alreadyRunning <- modifyMVar logDaemonLock $ \b -> pure (True, b)
    when alreadyRunning $ error "FATAL: tried to call logDaemon twice!"
    unsafeLogDaemon cfg

{-# NOINLINE logDaemonLock #-}
logDaemonLock :: MVar Bool
logDaemonLock = unsafePerformIO $ newMVar False

unsafeLogDaemon :: LogConfig -> IO (MsgDaemon LogEntry)
unsafeLogDaemon cfg = do
    msgDaemon logMsg "logger" logMsg (const $ pure ()) False
  where
    logMsg (LogEntry NOLOG _)        = pure ()
    logMsg (LogEntry level msg)      = when (level >= cfg ^. logLevel) $ do
                                            now <- getCurrentTime
                                            hPutStrLn stderr (cshow now <> " [" <> cshow level <> "] " <> cs msg)
    logMsg (LogEntryForModerator ev) = LBS.appendFile (cfg ^. eventLogPath) $ Aeson.encode ev <> cs "\n"

cleanUpDaemon :: SystemLogger -> CleanUpConfig -> TimeoutDaemon
cleanUpDaemon logger CleanUpConfig{..} =
    timeoutDaemon' logger "CLEANUP" _cleanUpInterval (mapM_ (cleanUpDir logger) _cleanUpRules)

cleanUpDir :: SystemLogger -> CleanUpRule -> IO ()
cleanUpDir logger CleanUpRule{..} = do
    files <- filterM doesFileExist =<< (_cleanUpDirectory </>) <$$> filter (_cleanUpPrefix `isPrefixOf`)
             <$> getDirectoryContents _cleanUpDirectory
    mods  <- mapM getModificationTime files
    let files' = drop _cleanUpKeepnum $ fst <$> reverseSortOn (to snd) (files `zip` mods)
    forM_ files' $ \file -> do
        logger . LogEntry INFO . cs $ "deleting file " <> file
        removeFile file

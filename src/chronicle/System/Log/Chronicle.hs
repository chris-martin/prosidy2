{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
module System.Log.Chronicle
    ( Level(..)
    , withLogger
    , log
    , trace
    , debug
    , info
    , warn
    , error
    , fatal
    )
where

import           Prelude                 hiding ( log
                                                , error
                                                )

import           Control.Concurrent             ( MVar
                                                , newEmptyMVar
                                                , tryReadMVar
                                                , tryPutMVar
                                                , takeMVar
                                                )
import           Control.Monad                  ( when )
import           Data.Char                      ( toUpper )
import           Data.Foldable                  ( for_ )
import           System.IO.Unsafe               ( unsafePerformIO )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )

import qualified Control.Exception             as Exception
import qualified System.Exit                   as Exit
import qualified System.Log.FastLogger         as Log
import qualified System.Log.FastLogger.Date    as Log.Date
import qualified Text.PrettyPrint.ANSI.Leijen  as PP
import qualified System.Console.ANSI           as ANSI
import qualified System.IO                     as IO

data Level =
      Trace
    | Debug
    | Info
    | Warn
    | Error
    | Fatal
  deriving (Show, Eq, Ord, Enum)

data Logger = Logger
    { color     :: Bool
    , minLevel  :: Maybe Level
    , loggerSet :: Log.LoggerSet
    , getTime   :: IO Log.FormattedTime
    }

globalLogger :: MVar Logger
globalLogger = unsafePerformIO newEmptyMVar

withLogger :: Maybe Bool -> Maybe Level -> IO a -> IO a
withLogger shouldColor minLevel =
    Exception.bracket install (Exception.mask_ . remove) . const . handler
  where
    handler :: IO x -> IO x
    handler = Exception.handle $ \e -> do
        case Exception.fromException @Exit.ExitCode e of
            Just exit -> do
                info @Log.LogStr "ExitCode exception was thrown; exiting"
                Exception.throwIO exit
            Nothing -> do
                error @String "An unhandled exception caused Prosidy to fail"
                fatal $ Exception.displayException e
                Exit.exitFailure

    install :: IO Bool
    install = do
        color     <- maybe (ANSI.hSupportsANSI IO.stderr) pure shouldColor
        loggerSet <- Log.newStderrLoggerSet Log.defaultBufSize
        getTime   <- Log.Date.newTimeCache "%Y-%m-%d %H:%M:%S"
        tryPutMVar globalLogger Logger { color, minLevel, loggerSet, getTime }

    remove :: Bool -> IO ()
    remove installed = when installed $ do
        Logger { loggerSet } <- takeMVar globalLogger
        Log.rmLoggerSet loggerSet

log :: Log.ToLogStr msg => Level -> msg -> IO ()
log level msg = do
    maybeLogger <- tryReadMVar globalLogger
    for_ maybeLogger $ \Logger { color, getTime, minLevel, loggerSet } ->
        when (maybe False (level >=) minLevel) $ do
            -- First, write out the formatted timestamp
            now <- getTime
            Log.pushLogStr loggerSet . Log.toLogStr $ now
            -- Then write out the log level, formatted to all capital letters
            let levelStr = fmap toUpper . show $ level
                levelPad = replicate (6 - length levelStr) ' '
            Log.pushLogStr loggerSet . Log.toLogStr $ levelPad
            Log.pushLogStr loggerSet . logColor color level . PP.text $ levelStr
            Log.pushLogStr loggerSet " | "
            -- Then write the message, followed by a new line
            Log.pushLogStrLn loggerSet . Log.toLogStr $ msg

logColor :: Bool -> Level -> PP.Doc -> Log.LogStr
logColor True  Fatal = docToLogStr . PP.bold . PP.white . PP.onred
logColor True  Error = docToLogStr . PP.bold . PP.red
logColor True  Warn  = docToLogStr . PP.bold . PP.yellow
logColor True  Info  = docToLogStr . PP.bold . PP.blue
logColor True  Debug = docToLogStr . PP.bold . PP.green
logColor True  Trace = docToLogStr . PP.magenta
logColor False _     = docToLogStr

docToLogStr :: PP.Doc -> Log.LogStr
docToLogStr = Log.toLogStr . flip PP.displayS "" . PP.renderPretty 1.0 maxBound

trace, debug, info, warn, error, fatal
    :: forall msg m . (MonadIO m, Log.ToLogStr msg) => msg -> m ()
trace = liftIO . log Trace
{-# INLINE trace #-}
{-# SPECIALIZE INLINE trace :: Log.ToLogStr msg => msg -> IO () #-}
debug = liftIO . log Debug
{-# INLINE debug #-}
{-# SPECIALIZE INLINE debug :: Log.ToLogStr msg => msg -> IO () #-}
info = liftIO . log Info
{-# INLINE info #-}
{-# SPECIALIZE INLINE info :: Log.ToLogStr msg => msg -> IO () #-}
warn = liftIO . log Warn
{-# INLINE warn #-}
{-# SPECIALIZE INLINE warn :: Log.ToLogStr msg => msg -> IO () #-}
error = liftIO . log Error
{-# INLINE error #-}
{-# SPECIALIZE INLINE error :: Log.ToLogStr msg => msg -> IO () #-}
fatal = liftIO . log Fatal
{-# INLINE fatal #-}
{-# SPECIALIZE INLINE fatal :: Log.ToLogStr msg => msg -> IO () #-}


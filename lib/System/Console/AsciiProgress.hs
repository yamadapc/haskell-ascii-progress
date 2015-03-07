{-# LANGUAGE RecordWildCards #-}
module System.Console.AsciiProgress
    ( ProgressBar(..)
    , Options(..)
    , Stats(..)
    , isComplete
    , newProgressBar
    , complete
    , tick
    , tickN
    , getProgressStrIO
    , getProgressStats
    , getProgressStr
    , registerLn
    -- Re-exports:
    , Default(..)
    )
  where

import Control.Applicative ((<$>))
import Control.Concurrent -- (readChan, readMVar, writeChan, modifyMVar_)
import Control.Concurrent.Async (Async, async, poll, wait)
import Data.Default (Default(..))
import Data.Maybe (isJust)
import System.Console.ANSI -- (clearLine, setCursorColumn)
import System.IO (BufferMode(..), hSetBuffering, stdout)
import System.IO.Unsafe (unsafePerformIO)

import System.Console.AsciiProgress.Internal

data ProgressBar = ProgressBar { pgInfo :: ProgressBarInfo
                               , pgFuture :: Async ()
                               }

nlines :: MVar Int
nlines = unsafePerformIO (newMVar 0)

writeLock :: MVar ()
writeLock = unsafePerformIO (newMVar ())

-- |
-- Registers a new line for multiple progress bars
registerLn :: IO ()
registerLn = modifyMVar_ nlines (\n -> return $ n + 1)

-- |
-- Creates a new progress bar with the given @Options@. Multiple progress bars
-- may be created as long as everytime a line is outputted by your program,
-- while progress bars run is followed by a call to `registerLn`
newProgressBar :: Options -> IO ProgressBar
newProgressBar opts = do
    hSetBuffering stdout NoBuffering
    info <- newProgressBarInfo opts
    cnlines <- modifyMVar nlines $ \nl -> return (nl + 1, nl)
    putStrLn ""
    future <- async $ start info cnlines
    return $ ProgressBar info future
  where
    resetCursor = clearLine >> setCursorColumn 0
    unlessDone _ c action | c < pgTotal opts = action
    unlessDone cnlines _ _ = atProgressLine cnlines (pgOnCompletion opts)

    atProgressLine cnlines action = do
        diff <- (\nl -> nl - cnlines) <$> readMVar nlines
        cursorUp diff
        resetCursor
        action
        cursorDown diff
        resetCursor

    start info@ProgressBarInfo{..} cnlines = do
       c <- readMVar pgCompleted
       unlessDone cnlines c $ do
           n <- readChan pgChannel
           handleMessage info cnlines n
           unlessDone cnlines (c + n) $
               start info cnlines

    handleMessage info cnlines n = modifyMVar_ writeLock $ const $ do
        -- Update the completed tick count
        modifyMVar_ (pgCompleted info) (\c -> return (c + n))
        -- Find and update the current and first tick times:
        stats <- getInfoStats info
        let progressStr = getProgressStr opts stats
        atProgressLine cnlines $
            putStr progressStr

-- |
-- Tick the progress bar
tick :: ProgressBar -> IO ()
tick pg = tickN pg 1

-- |
-- Tick the progress bar N times
tickN :: ProgressBar -> Int -> IO ()
tickN (ProgressBar info _) = writeChan (pgChannel info)

-- |
-- Returns if the progress bar rendering thread has exited (it has done enough
-- ticks)
isComplete :: ProgressBar -> IO Bool
isComplete (ProgressBar _ future) = isJust <$> poll future

-- |
-- Forces a 'ProgressBar' to finish
complete :: ProgressBar -> IO ()
complete pg@(ProgressBar info future) = do
    let total = pgTotal (pgOptions info)
    tickN pg total
    wait future

-- |
-- Gets the progress bar current @Stats @object
getProgressStats :: ProgressBar -> IO Stats
getProgressStats (ProgressBar info _) = getInfoStats info

-- |
-- Like @getProgressStr@ but works on the @ProgressBar@ object and uses the IO
-- monad.
getProgressStrIO :: ProgressBar -> IO String
getProgressStrIO (ProgressBar info _) =
    getProgressStr (pgOptions info) <$> getInfoStats info

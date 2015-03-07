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
    -- Re-exports:
    , Default(..)
    )
  where

import Control.Applicative ((<$>))
import Control.Concurrent (readChan, readMVar, writeChan, modifyMVar_)
import Control.Concurrent.Async (Async, async, poll, wait)
import Data.Default (Default(..))
import Data.Maybe (isJust)
import System.Console.ANSI (clearLine, setCursorColumn)
import System.IO (BufferMode(..), hSetBuffering, stdout)

import System.Console.AsciiProgress.Internal

data ProgressBar = ProgressBar ProgressBarInfo (Async ())

-- |
-- Creates a new progress bar with the given @Options@
newProgressBar :: Options -> IO ProgressBar
newProgressBar opts = do
    hSetBuffering stdout NoBuffering
    info <- newProgressBarInfo opts
    future <- async $ start info
    return $ ProgressBar info future
  where
    start info@ProgressBarInfo{..} = do
        c <- readMVar pgCompleted
        if c < pgTotal opts
            then do
                n <- readChan pgChannel
                handleMessage n
                if c + n < pgTotal opts
                    then start info
                    else reset
            else reset
      where
        reset = do
            clearLine
            setCursorColumn 0
        handleMessage n = do
            -- Update the completed tick count
            modifyMVar_ pgCompleted (\c -> return (c + n))
            -- Find and update the current and first tick times:
            stats <- getInfoStats info
            reset
            let progressStr = getProgressStr opts stats
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

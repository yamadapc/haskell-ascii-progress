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
    , getProgressTxtIO
    , getProgressStats
    , getProgressTxt
    , registerLn
    -- Formatters
    , ProgressFormat
    , eta
    , elapsed
    , total
    , percent
    , current
    , bar
    -- Re-exports:
    , Default(..)
    , (%)
    , (%.)
    , (<>)
    )
  where

import Control.Applicative ((<$>))
import Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar, readChan,
                           readMVar, withMVarMasked, writeChan)
import Control.Concurrent.Async (Async, async, poll, wait)
import Data.Default (Default(..))
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text.IO as T (putStr, putStrLn)
import Formatting ((%), (%.))
import System.Console.ANSI (clearLine, cursorDown, cursorUp, setCursorColumn)
import System.IO (BufferMode(..), hSetBuffering, stdout)
import System.IO.Unsafe (unsafePerformIO)

import System.Console.AsciiProgress.Internal

data ProgressBar = ProgressBar { pgInfo :: ProgressBarInfo
                               , pgFuture :: Async ()
                               }

-- |
-- Creates a new progress bar with the given @Options@. Multiple progress bars
-- may be created as long as everytime a line is outputted by your program,
-- while progress bars run is followed by a call to `registerLn`
newProgressBar :: Options -> IO ProgressBar
newProgressBar opts = withWriteLock $ do
    hSetBuffering stdout NoBuffering
    info <- newProgressBarInfo opts
    cnlines <- modifyMVar nlines $ \nl -> return (nl + 1, nl)
    getProgressTxt opts <$> getInfoStats info >>= T.putStrLn
    future <- async $ start info cnlines
    return $ ProgressBar info future
  where
    resetCursor = clearLine >> setCursorColumn 0
    unlessDone _ c action | c < pgTotal opts = action
    unlessDone cnlines _ _ = atProgressLine cnlines (pgOnCompletion opts)

    atProgressLine cnlines action = withWriteLock $ do
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

    handleMessage info cnlines n = do
        -- Update the completed tick count
        modifyMVar_ (pgCompleted info) (\c -> return (c + n))
        -- Find and update the current and first tick times:
        stats <- getInfoStats info
        let progressTxt = getProgressTxt opts stats
        atProgressLine cnlines $
            T.putStr progressTxt

nlines :: MVar Int
nlines = unsafePerformIO (newMVar 0)

writeLock :: MVar ()
writeLock = unsafePerformIO (newMVar ())

withWriteLock :: IO a -> IO a
withWriteLock action = withMVarMasked writeLock (const action)

-- |
-- Registers a new line for multiple progress bars
registerLn :: IO ()
registerLn = modifyMVar_ nlines (\n -> return $ n + 1)


-- |
-- Tick the progress bar
tick :: ProgressBar -> IO ()
tick pg = tickN pg (1 :: Integer)

-- |
-- Tick the progress bar N times
tickN :: Integral a => ProgressBar -> a -> IO ()
tickN (ProgressBar info _) = writeChan (pgChannel info) . fromIntegral

-- |
-- Returns if the progress bar rendering thread has exited (it has done enough
-- ticks)
isComplete :: ProgressBar -> IO Bool
isComplete (ProgressBar _ future) = isJust <$> poll future

-- |
-- Forces a 'ProgressBar' to finish
complete :: ProgressBar -> IO ()
complete pg@(ProgressBar info future) = do
    let tot = pgTotal (pgOptions info)
    tickN pg tot
    wait future

-- |
-- Gets the progress bar current @Stats @object
getProgressStats :: ProgressBar -> IO Stats
getProgressStats (ProgressBar info _) = getInfoStats info

-- |
-- Like @getProgressStr@ but works on the @ProgressBar@ object and uses the IO
-- monad.
getProgressTxtIO :: ProgressBar -> IO Text
getProgressTxtIO (ProgressBar info _) =
    getProgressTxt (pgOptions info) <$> getInfoStats info

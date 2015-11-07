
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
    , module System.Console.Regions
    , module System.Console.Concurrent
    )
  where
import           Control.Applicative                   ((<$>))
import           Control.Concurrent                    (MVar, forkIO,
                                                        modifyMVar, modifyMVar_,
                                                        newMVar, readChan,
                                                        readMVar, writeChan)
import           Control.Concurrent.Async
import           Debug.Trace
-- (Async, async, poll,
                                                        -- wait)
import           Data.Default                          (Default (..))
import           Data.Maybe                            (fromMaybe, isJust)
import           System.Console.ANSI                   (clearLine, cursorDown,
                                                        cursorUp,
                                                        setCursorColumn)
import           System.Console.AsciiProgress.Internal
import           System.Console.Concurrent
import           System.Console.Regions
import           System.IO                             (BufferMode (..),
                                                        hPutStrLn,
                                                        hSetBuffering, stderr,
                                                        stdout)
import           System.IO.Unsafe                      (unsafePerformIO)

data ProgressBar = ProgressBar { pgInfo   :: ProgressBarInfo
                               , pgFuture :: Async ()
                               }

nlines :: MVar Int
nlines = unsafePerformIO (newMVar 0)

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
    region <- openConsoleRegion Linear
    info <- newProgressBarInfo opts

    -- Display initial progress-bar
    pgStr <- getProgressStr opts <$> getInfoStats info
    setConsoleRegion region pgStr

    future <- async $ start info region
    return $ ProgressBar info future
  where
    start info@ProgressBarInfo{..} region = do
       c <- readMVar pgCompleted
       unlessDone c $ do
           n <- readChan pgChannel
           _ <- handleMessage info region n
           unlessDone (c + n) $ start info region
      where
        unlessDone c action | c < pgTotal opts = action
        unlessDone _ _ = do
            let fmt = fromMaybe (pgFormat opts) (pgOnCompletion opts)
            onCompletion <- getProgressStr opts { pgFormat = fmt } <$> getInfoStats info
            finishConsoleRegion region onCompletion

    handleMessage info region n = do
        -- Update the completed tick count
        modifyMVar_ (pgCompleted info) (\c -> return (c + n))
        -- Find and update the current and first tick times:
        stats <- getInfoStats info
        let progressStr = getProgressStr opts stats
        setConsoleRegion region progressStr

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

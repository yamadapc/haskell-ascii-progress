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
    , tickNI
    , getProgressStrIO
    , getProgressStats
    , getProgressStr
    -- Re-exports:
    , Default(..)
    , module System.Console.Regions
    )
  where

import           Control.Applicative                   ((<$>))
import           Control.Concurrent                    (modifyMVar_, readChan,
                                                        readMVar, writeChan)
import           Control.Concurrent.Async              (Async (..), async, poll,
                                                        wait)
import           Data.Default                          (Default (..))
import           Data.Maybe                            (fromMaybe, isJust)
import           System.Console.AsciiProgress.Internal
import           System.Console.Regions

data ProgressBar = ProgressBar { pgInfo   :: ProgressBarInfo
                               , pgFuture :: Async ()
                               , pgRegion :: ConsoleRegion
                               }

-- |
-- Creates a new progress bar with the given @Options@. Multiple progress bars
-- may be created. This package depends on `concurrent-output`, so it's --
-- necessary that progress-bar usage is wrapped with a call to
-- 'displayConsoleRegions'.
--
-- > import           Control.Concurrent           (threadDelay)
-- > import           Control.Monad                (unless)
-- > import           System.Console.AsciiProgress
-- >
-- > main :: IO ()
-- > main = do
-- >    pg <- newProgressBar def { pgWidth = 100
-- >                             , pgOnCompletion = Just "Done :percent after :elapsed seconds"
-- >                             }
-- >    loop pg
-- >  where
-- >    loop pg = do
-- >        b <- isComplete pg
-- >        unless b $ do
-- >            threadDelay $ 200 * 1000
-- >            tick pg
-- >            loop pg
newProgressBar :: Options -> IO ProgressBar
newProgressBar opts = do
    region <- openConsoleRegion Linear
    info <- newProgressBarInfo opts

    -- Display initial progress-bar
    pgStr <- getProgressStr opts <$> getInfoStats info
    setConsoleRegion region pgStr

    future <- async $ start info region
    return $ ProgressBar info future region
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
            setConsoleRegion region onCompletion

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
tickN (ProgressBar info _ _) = writeChan (pgChannel info) . fromIntegral

-- |
-- Tick the progress bar N times
tickNI :: ProgressBar -> Integer -> IO ()
tickNI (ProgressBar info _ _) = writeChan (pgChannel info)

-- |
-- Returns if the progress bar rendering thread has exited (it has done enough
-- ticks)
isComplete :: ProgressBar -> IO Bool
isComplete (ProgressBar _ future _) = isJust <$> poll future

-- |
-- Forces a 'ProgressBar' to finish
complete :: ProgressBar -> IO ()
complete pg@(ProgressBar info future _) = do
    let total = pgTotal (pgOptions info)
    tickNI pg total
    wait future

-- |
-- Gets the progress bar current @Stats @object
getProgressStats :: ProgressBar -> IO Stats
getProgressStats (ProgressBar info _ _) = getInfoStats info

-- |
-- Like @getProgressStr@ but works on the @ProgressBar@ object and uses the IO
-- monad.
getProgressStrIO :: ProgressBar -> IO String
getProgressStrIO (ProgressBar info _ _) =
    getProgressStr (pgOptions info) <$> getInfoStats info

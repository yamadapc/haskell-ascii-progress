{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module System.Console.AsciiProgress.Internal
  where

import Control.Concurrent (Chan, MVar, newChan, newEmptyMVar, newMVar,
                           readMVar, tryPutMVar, tryReadMVar)
import Data.Default (Default(..))
import Data.List.Utils (replace)
import Data.Time.Clock
import Text.Printf

-- |
-- The progress bar's options.
data Options = Options { pgFormat :: String
                       -- ^ A format string for the progress bar. Currently the
                       -- following format strings are supported:
                       -- - ":eta" (ETA displayed in seconds)
                       -- - ":current" (current tick)
                       -- - ":total" (total number of ticks)
                       -- - ":percent" (percentage completed)
                       -- - ":elapsed" (elapsed time in seconds)
                       -- - ":bar" (the actual progress bar)
                       , pgCompletedChar :: Char
                       -- ^ Character to be used on the completed part of the
                       -- bar
                       , pgPendingChar :: Char
                       -- ^ Character to be used on the pending part of the bar
                       , pgTotal :: Int
                       -- ^ Total amount of ticks expected
                       , pgWidth :: Int
                       -- ^ The progress bar's width
                       }
  deriving(Eq, Ord, Show)

instance Default Options where
    def = Options { pgFormat = "Working :percent [:bar] :current/:total " ++
                               "(for :elapsed, :eta remaining)"
                  , pgCompletedChar = '='
                  , pgPendingChar = ' '
                  , pgTotal = 20
                  , pgWidth = 80
                  }

-- |
-- The progress bar's state object. Contains all but the printing thread's
-- @Async@ object.
data ProgressBarInfo = ProgressBarInfo { pgOptions :: Options
                                       , pgChannel :: Chan Int
                                       , pgCompleted :: MVar Int
                                       , pgFirstTick :: MVar UTCTime
                                       }
  deriving(Eq)

-- |
-- Represents a point in time for the progress bar.
data Stats = Stats { stTotal :: Int
                   , stCompleted :: Int
                   , stRemaining :: Int
                   , stElapsed :: Double
                   , stPercent :: Double
                   , stEta :: Double
                   }

-- |
-- Creates a new empty progress bar info object.
newProgressBarInfo :: Options -> IO ProgressBarInfo
newProgressBarInfo opts = do
    chan <- newChan
    mcompleted <- newMVar 0
    mfirstTick <- newEmptyMVar
    return $ ProgressBarInfo opts chan mcompleted mfirstTick

-- |
-- Gets the string to be printed given the options object and a certain stats
-- object representing the rendering moment.
getProgressStr :: Options -> Stats -> String
getProgressStr Options{..} Stats{..} = replace ":bar" barStr statsStr
  where
    statsStr = replaceMany
        [ (":elapsed", printf "%3.1f" stElapsed)
        , (":current", printf "%3d"   stCompleted)
        , (":total"  , printf "%3d"   stTotal)
        , (":percent", printf "%3d%%" (floor (100 * stPercent) :: Int))
        , (":eta"    , printf "%3.1f" stEta)
        ]
        pgFormat
    barWidth = pgWidth - length (replace ":bar" "" statsStr)
    barStr   = getBar pgCompletedChar pgPendingChar barWidth stPercent

-- |
-- Creates a stats object for a given @ProgressBarInfo@ node. This is the core
-- logic, isolated, and may be used to make the same analysis code to be used
-- by different progress renderers.
getInfoStats :: ProgressBarInfo -> IO Stats
getInfoStats info = do
    completed   <- readMVar (pgCompleted info)
    currentTime <- getCurrentTime
    initTime    <- forceReadMVar (pgFirstTick info) currentTime
    let total     = pgTotal (pgOptions info)
        remaining = total - completed
        elapsed   = getElapsed initTime currentTime
        percent   = fromIntegral completed / fromIntegral total
        eta       = getEta completed remaining elapsed
    return $ Stats total completed remaining elapsed percent eta

-- |
-- Generates the actual progress bar string, with its completed/pending
-- characters, width and a completeness percentage.
getBar :: Char -> Char -> Int -> Double -> String
getBar completedChar pendingChar width percent =
    replicate bcompleted completedChar ++ replicate bremaining pendingChar
  where
    percentRemaining = 1 - percent
    fwidth = fromIntegral width
    bcompleted = ceiling $ fwidth * percent
    bremaining = floor   $ fwidth * percentRemaining

-- |
-- Gets the amount of seconds elapsed between two @UTCTime@s as a double.
getElapsed :: UTCTime -> UTCTime -> Double
getElapsed initTime currentTime = realToFrac (diffUTCTime currentTime initTime)

-- |
-- Gets the ETA, given the elapsed time and the amount of completed and
-- remaining ticks.
--
-- >>> getEta 50 50 10.0
-- 10.0
-- >>> getEta 30 70 23.3
-- 54.366666666666674
getEta :: Int -> Int -> Double -> Double
getEta completed remaining elapsed = averageSecsPerTick * fromIntegral remaining
  where
    averageSecsPerTick = elapsed / fromIntegral completed

-- |
-- Replaces each pair in a list of replacement pairs in a list with replace.
-- The idea is to call @(\(old, new) target -> replace old new target)@ on each
-- of the pairs, accumulating the resulting modified list.
--
-- >>> replaceMany [] "foobar"
-- "foobar"
-- >>> replaceMany [("bar", "biz")] "foobar"
-- "foobiz"
-- >>> replaceMany [("foo", "baz"), ("bar", "biz")] "foobar"
-- "bazbiz"
replaceMany :: Eq a => [([a], [a])] -> [a] -> [a]
replaceMany pairs target = foldr (uncurry replace) target pairs

-- |
-- Forces an MVar's contents to be read or swaped by a default value, even if
-- it's currently empty. Will discard the default value write to the MVar if it
-- becomes full in the middle of the operation and return its value. It's
-- assumed that once the MVar becomes full, it won't ever be left emptied. This
-- code may deadlock if that's the case.
forceReadMVar :: MVar a -> a -> IO a
forceReadMVar mv v = tryReadMVar mv >>= \case
    Nothing -> do
        success <- tryPutMVar mv v
        if success
           then return v
           else readMVar mv
    Just o -> return o

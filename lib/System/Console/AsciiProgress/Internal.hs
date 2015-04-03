{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
module System.Console.AsciiProgress.Internal
  where

import Control.Concurrent (Chan, newChan, newEmptyMVar, newMVar,
                           readMVar, tryPutMVar)
import Data.Default (Default(..))
import Data.Monoid ((<>))
import Data.Time.Clock
import Data.Text (Text, unpack)
import Data.Text.Buildable (build)
import Formatting hiding (build)
import GHC.Base (IO(..), tryReadMVar#)
import GHC.MVar (MVar(..))
import Text.Printf

-- |
-- The progress bar's options.
data Options = Options { pgFormat :: Format Text (Stats -> Text)
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
                       , pgTotal :: Integer
                       -- ^ Total amount of ticks expected
                       , pgWidth :: Integer
                       -- ^ The progress bar's width
                       , pgOnCompletion :: IO ()
                       -- ^ An IO action to be executed on completion, with the
                       -- cursor set at progress bar's line
                       }

instance Default Options where
    def = Options { pgFormat = "Working " % percent <> " [" % bar % "] " <>
                               current % "/" <> total %
                               " (for " <> elapsed % ", " <> eta % " remaining)"
                  , pgCompletedChar = '='
                  , pgPendingChar = ' '
                  , pgTotal = 20
                  , pgWidth = 80
                  , pgOnCompletion = return ()
                  }

-- |
-- The progress bar's state object. Contains all but the printing thread's
-- @Async@ object.
data ProgressBarInfo = ProgressBarInfo { pgOptions :: Options
                                       , pgChannel :: Chan Integer
                                       , pgCompleted :: MVar Integer
                                       , pgFirstTick :: MVar UTCTime
                                       }

-- |
-- Represents a point in time for the progress bar.
data Stats = Stats { stTotal :: Integer
                   , stCompleted :: Integer
                   , stRemaining :: Integer
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
getProgressStr Options{..} st@Stats{..} = replace ":bar" barStr statsStr
  where
    statsStr = unpack (sformat pgFormat st)
    barWidth = pgWidth - fromIntegral (length (replace ":bar" "" statsStr))
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
    let to = pgTotal (pgOptions info)
        re = to - completed
        el = getElapsed initTime currentTime
        pe = fromIntegral completed / fromIntegral to
        et = getEta completed re el
    return $ Stats to completed re el pe et

-- |
-- Generates the actual progress bar string, with its completed/pending
-- characters, width and a completeness percentage.
getBar :: Char -> Char -> Integer -> Double -> String
getBar completedChar pendingChar width per =
    replicate (fromInteger bcompleted) completedChar ++ replicate (fromInteger bremaining) pendingChar
  where
    fwidth = fromIntegral width
    bcompleted = ceiling $ fwidth * per
    bremaining = width - bcompleted

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
getEta :: Integer -> Integer -> Double -> Double
getEta completed remaining ela = averageSecsPerTick * fromIntegral remaining
  where
    averageSecsPerTick = ela / fromIntegral completed

laterBuild :: Buildable b => (a -> b) -> Format r (a -> r)
laterBuild f = later (build . f)

slaterBuild :: Buildable b => (a -> c) -> (c -> b) -> Format r (a -> r)
slaterBuild p f = laterBuild (f . p)

eta :: Format r (Stats -> r)
eta = slaterBuild stEta (printf "%5.1f" :: Double -> String)

elapsed :: Format r (Stats -> r)
elapsed = slaterBuild stElapsed (printf "%5.1f" :: Double -> String)

total :: Format r (Stats -> r)
total = slaterBuild stTotal (printf "%3d" :: Integer -> String)

percent :: Format r (Stats -> r)
percent = slaterBuild stPercent $
              (printf "%3d%%" :: Int -> String) . floor . (100 *)

current :: Format r (Stats -> r)
current = slaterBuild stCompleted (printf "%3d" :: Integer -> String)

bar :: Format Text (Stats -> Text)
bar = laterBuild $ const (":bar" :: Text)

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
-- Replaces a subsequence by another in a sequence
--
-- Taken from http://bluebones.net/2007/01/replace-in-haskell/
--
-- >>> replace "foo" "baz" "foobar"
-- "bazbar"
-- >>> replace "some" "thing" "something something"
-- "thingthing thingthing"
-- >>> replace "not" "" "something"
-- "something"
-- >>> replace "" "here" "something"
-- "heresomething"
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace _ _ [] = []
replace [] new target = new ++ target
replace old new target@(t:ts) =
  if take len target == old
      then new ++ replace old new (drop len target)
      else t : replace old new ts
  where len = length old

-- |
-- Forces an MVar's contents to be read or swaped by a default value, even if
-- it's currently empty. Will discard the default value write to the MVar if it
-- becomes full in the middle of the operation and return its value. It's
-- assumed that once the MVar becomes full, it won't ever be left emptied. This
-- code may deadlock if that's the case.
forceReadMVar :: MVar a -> a -> IO a
forceReadMVar mv v = tryReadMVar mv >>= \m -> case m of
    Nothing -> do
        success <- tryPutMVar mv v
        if success
           then return v
           else readMVar mv
    Just o -> return o

-- Monkey patch base < 4.7
-- |A non-blocking version of 'readMVar'.  The 'tryReadMVar' function
-- returns immediately, with 'Nothing' if the 'MVar' was empty, or
-- @'Just' a@ if the 'MVar' was full with contents @a@.
--
-- /Since: 4.7.0.0/
tryReadMVar :: MVar a -> IO (Maybe a)
tryReadMVar (MVar m) = IO $ \s ->
    case tryReadMVar# m s of
        (# s', 0#, _ #) -> (# s', Nothing #)      -- MVar is empty
        (# s', _,  a #) -> (# s', Just a  #)      -- MVar is full

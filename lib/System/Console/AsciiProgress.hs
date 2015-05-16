{-# LANGUAGE RecordWildCards #-}
-- |
-- Module: System.Console.AsciiProgress
-- Description: A simple progress bar for the console.
-- Copyright: (c) 2015 Pedro Tacla Yamada
-- License: MIT
-- Maintainer: tacla.yamada@gmail.com
-- Stability: experimental
-- Portability: portable
--
-- A simple Haskell progress bar for the console. It heavily borrows from TJ
-- Holowaychuk's Node.JS project
-- <https://github.com/tj/node-progress progress>.
-- See the
-- <https://github.com/yamadapc/haskell-ascii-progress README on our GitHub page>
-- for visual demos to see how progress bars look. There are also several examples
-- there.
--
-- = Getting started (in 5 minutes or less)
-- There are only two basic ideas here. The 'ProgressBar' object and ticking it
-- using the 'tick' or 'tickN' functions. First, we create a 'ProgressBar' with
-- 'newProgressBar', setting how many ticks its going to have before being
-- completed and then we tick it away wherever we want.
--
-- == Creating new progress bars
-- The 'newProgressBar' function takes an 'Options' object, but an instance to
-- the 'Default' class from
-- <https://hackage.haskell.org/package/data-default data-default>
-- is implemented, so usage is kept simple.
--
-- This is how to create a 'ProgressBar' with 50 ticks:
--
-- @
-- pg <- newProgressBar def { pgTotal = 50 }
-- @
--
-- Multiple progress bars may be created and updated concurrently, so Docker
-- style progress interfaces are both possible and easy to implement.
--
-- == Updating your progress bar
--
-- @
-- tick pg -- Ticks the progress bar once
-- tickN pg 10 -- Ticks the progress bar 10 times:
-- @
--
-- = Customizing how it looks
-- For the most part, the documentation on 'Options' should be more than enough
-- to understand how to customize @ascii-progress@. There are fields for
-- setting:
--
--     - the total number of ticks in the progress bar ('pgTotal')
--     - the width in characters the progress bar will take ('pgWidth')
--     - the completion character ('pgCompletedChar')
--     - the pending character ('pgPendingChar')
--     - an 'IO' action to be ran when the progress bar is completed, on its line
--       ('pgOnCompletion')
--     - the format of the progress bar, which brings us to the next section
--       ('pgFormat')
--
-- == About 'pgFormat', what gets displayed and how
-- @ascii-progress@ comes built in with support for displaying things like the
-- elapsed time, the current tick, the ETA and more. There are two ways to
-- specify it to the library:
--
-- === Using a 'Text' printf-style format string
-- @
-- def { pgFormat = "[:bar] for :elapsed seconds" }
-- -- Which will be something like: "[========            ] for  3.2 seconds"
-- @
--
-- === Using a 'ProgressFormat', which is just an alias to @Format Text (Stats -> Text)@
-- @
-- def { pgFormat = "[" % bar % "] " <> elapsed }
-- -- Same as above, but type-safe and extensible
-- @
--
-- == Wait 'Stats' what?
-- @ascii-progress@ uses a 'Stats' type to represent the data required for each
-- render. It then exposes several formatters which just introspect into the
-- type. You may both define your own formatters, by looking at the type, and
-- manually get the 'Stats' object with 'getProgressStats'.
--
-- == Defining more formatters
-- The 'System.Console.AsciiProgress.Formatting' module exports formatters for
-- showing the current and total tick amounts as filesizes. It's very easy to
-- define these kinds of formatters using this module.
--
-- Please take a look at
-- <https://github.com/yamadapc/haskell-ascii-progress/blob/master/lib/System/Console/AsciiProgress/Formatting.hs the source code>
-- for examples for better information.
module System.Console.AsciiProgress
    (
    -- * Basic functions and types
      ProgressBar(..)
    , newProgressBar
    , tick
    , tickN
    -- ** Querying for completion
    , isComplete
    , complete
    -- ** Getting the progress bar state in time
    , Stats(..)
    , getProgressStats
    -- * Options
    , Options(..)
    -- ** Formatters
    , ProgressFormat
    -- *** Base formatters
    , eta
    , elapsed
    , total
    , percent
    , current
    , bar
    -- *** Extra Formatters
    , currentFilesize
    , totalFilesize
    -- ** Manually getting the progress bar string representation
    , getProgressTxt
    , getProgressTxtIO
    -- * Helpers for concurrent progress bars
    , registerLn
    -- * Re-exports for convenience
    , Default(..)
    , (%)
    , (%.)
    , (<>)
    )
  where

import Control.Applicative ((<$>))
import Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar, readChan,
                           readMVar, withMVar, writeChan)
import Control.Concurrent.STM (atomically, modifyTVar, readTVarIO)
import Control.Concurrent.Async (Async, async, poll, wait)
import Control.Monad (void)
import Data.Default (Default(..))
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text.IO as T (putStr, putStrLn)
import Formatting ((%), (%.))
import System.Console.ANSI (clearLine, cursorDown, cursorUp, setCursorColumn)
import System.IO (BufferMode(..), hSetBuffering, stdout)
import System.IO.Unsafe (unsafePerformIO)

import System.Console.AsciiProgress.Formatting
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
       c <- readTVarIO pgCompleted
       unlessDone cnlines c $ do
           n <- readChan pgChannel
           handleMessage info cnlines n
           unlessDone cnlines (c + n) $
               start info cnlines

    handleMessage info cnlines n = do
        -- Update the completed tick count
        void $ atomically $ modifyTVar (pgCompleted info) (+ n)
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
withWriteLock action = withMVar writeLock (const action)

-- |
-- Registers a new line for multiple progress bars
registerLn :: IO ()
registerLn = modifyMVar_ nlines (\n -> return $ n + 1)


-- |
-- Tick the progress bar
tick :: ProgressBar -> IO ()
tick pg = tickN pg 1

-- |
-- Tick the progress bar N times
tickN :: ProgressBar -> Integer -> IO ()
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

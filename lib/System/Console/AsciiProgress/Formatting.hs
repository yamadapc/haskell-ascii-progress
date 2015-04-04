{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: System.Console.AsciiProgress.Formatting
-- Copyright: (c) 2015 Pedro Tacla Yamada
-- License: MIT
-- Description: Formatting helpers for ascii-progress, borrows from Humanize.js
-- Maintainer: tacla.yamada@gmail.com
-- Stability: experimental
-- Portability: portable
module System.Console.AsciiProgress.Formatting
  where

import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Formatting

import System.Console.AsciiProgress.Internal

-- |
-- The current number of ticks as a filesize.
currentFilesize :: ProgressFormat
currentFilesize = slaterBuild stCurrent humanFilesize

-- |
-- The current number of ticks as a filesize.
totalFilesize :: ProgressFormat
totalFilesize = slaterBuild stTotal humanFilesize

-- |
-- Formats an integer as a filesize value.
humanFilesize :: Integer -> Text
humanFilesize = humanInteger ["B", "KB", "MB", "GB", "TB", "PB"] 1024


-- |
-- Formats an integer in a human-readable form, abbreviating orders of measure
-- as appropriate.
humanInteger :: [Text]
             -- ^ A list of units of measure
             -> Integer
             -- ^ A multiplication step between each unit
             -> Integer
             -- ^ The number to format
             -> Text
humanInteger units step n | n < step = pack (show n) <> if null units
                                           then ""
                                           else head units
humanInteger units step n = helper (fromInteger n) 0
  where
    len = length units
    fstep = fromInteger step
    helper n i | i + 1 < len = let n' = n / fstep
                                 in if n' < fstep
                                        then sformat (fixed 1) n' <> units !! (i + 1)
                                        else helper n' (i + 1)
    helper n i = sformat (fixed 1) n <> last units

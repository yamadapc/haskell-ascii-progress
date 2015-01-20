module System.Console.AsciiProgress
    (
      ProgressBar(..)
    , Options(..)
    , complete
    , newProgressBar
    , tick
    , tickN
    , getProgressStr
    -- Re-exports:
    , Default(..)
    )
  where

import Control.Applicative ((<$>))
import Control.Concurrent (Chan, MVar, modifyMVar, newChan, newMVar, readChan,
                           readMVar, writeChan)
import Control.Concurrent.Async (Async, async, poll)
import Control.Monad (when)
import Data.Default (Default(..))
import Data.Maybe (isJust)
import Data.List.Utils (replace)
import System.Console.ANSI (setCursorColumn)
import System.IO (BufferMode(..), hSetBuffering, stdout)

data ProgressBar = ProgressBar { pgFuture  :: Async ()
                               , pgChannel :: Chan (Maybe Int)
                               , pgOptions :: Options
                               , pgComplete :: MVar Int
                               }
  deriving(Eq)

data Options = Options { pgFormat :: String
                       , pgCompletedChar :: Char
                       , pgPendingChar :: Char
                       , pgTotal :: Int
                       , pgWidth :: Int
                       }
  deriving(Eq, Ord, Show)

instance Default Options where
    def = Options { pgFormat = "[:bar]"
                  , pgCompletedChar = '='
                  , pgPendingChar = ' '
                  , pgTotal = 20
                  , pgWidth = 80
                  }

newProgressBar :: Options -> IO ProgressBar
newProgressBar opts = do
    hSetBuffering stdout NoBuffering
    mcompleted <- newMVar 0
    chan <- newChan
    future <- async $ start mcompleted chan
    return $ ProgressBar future chan opts mcompleted
  where
    start mcompleted chan = do
        c <- readMVar mcompleted
        when (c < pgTotal opts) $ do
            m <- readChan chan
            handleMessage mcompleted m
            start mcompleted chan
    handleMessage mcompleted (Just n) = do
        c <- modifyMVar mcompleted (\c -> let c' = c + n in return (c', c'))
        render opts c
    handleMessage mcompleted Nothing = handleMessage mcompleted (Just 1)

render :: Options -> Int -> IO ()
render opts completed = do
    let progressStr = getProgressStr opts completed
    setCursorColumn 0
    putStr progressStr

getProgressStr :: Options -> Int -> String
getProgressStr opts completed =
    let fmt = pgFormat opts
        -- Use TJ Holowaychuk's approach
        barWidth = pgWidth opts - length (replace ":bar" "" fmt)
        barStr = getBarStr (pgCompletedChar opts) (pgPendingChar opts) barWidth
                           percentCompleted
      in replace ":bar" barStr fmt
  where
    percentCompleted = (fromIntegral completed :: Double) /
                       fromIntegral (pgTotal opts)

complete :: ProgressBar -> IO Bool
complete pg = isJust <$> poll (pgFuture pg)

tick :: ProgressBar -> IO ()
tick pg = writeChan (pgChannel pg) Nothing

tickN :: ProgressBar -> Int -> IO ()
tickN pg n = writeChan (pgChannel pg) (Just n)

getBarStr :: (RealFrac s, Integral a) => Char -> Char -> a -> s -> String
getBarStr completedChar pendingChar width percentCompleted =
    replicate bcompleted completedChar ++ replicate bremaining pendingChar
  where
    percentRemaining = 1 - percentCompleted
    bcompleted = percentToBlockSize percentCompleted
    bremaining = percentToBlockSize percentRemaining
    percentToBlockSize p = floor $ fromIntegral width * p

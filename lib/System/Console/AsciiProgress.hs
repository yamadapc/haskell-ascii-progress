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
import Control.Concurrent -- (Chan, MVar, modifyMVar, newChan, newEmptyMVar,
                           -- newMVar, readChan, readMVar, writeChan, modifyMVar_)
import Control.Concurrent.Async (Async, async, poll)
import Control.Monad (when)
import Data.Default (Default(..))
import Data.Maybe (isJust)
import Data.List.Utils (replace)
import Data.Time.Clock
import Text.Printf
import System.Console.ANSI (clearLine, setCursorColumn)
import System.IO (BufferMode(..), hSetBuffering, stdout)

data ProgressBar = ProgressBar { pgFuture  :: Async ()
                               , pgChannel :: Chan (Maybe Int)
                               , pgOptions :: Options
                               , pgCompleted :: MVar Int
                               , pgLastTick :: MVar UTCTime
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
    def = Options { pgFormat = "Working :percent [:bar] :current/:total " ++
                               "(for :elapsed, :eta remaining)"
                  , pgCompletedChar = '='
                  , pgPendingChar = ' '
                  , pgTotal = 20
                  , pgWidth = 80
                  }

newProgressBar :: Options -> IO ProgressBar
newProgressBar opts = do
    hSetBuffering stdout NoBuffering
    mcompleted <- newMVar 0
    mlastTick <- newEmptyMVar
    chan <- newChan
    initTime <- getCurrentTime
    future <- async $ start mlastTick mcompleted chan initTime
    return $ ProgressBar future chan opts mcompleted mlastTick
  where
    start mlastTick mcompleted chan initTime = do
        c <- readMVar mcompleted
        when (c < pgTotal opts) $ do
            readChan chan >>= handleMessage
            start mlastTick mcompleted chan initTime
        clearLine
        setCursorColumn 0
      where
        handleMessage (Just n) = do
            completed <- modifyMVar mcompleted (\c -> let c' = c + n in return (c', c'))
            currentTime <- getCurrentTime
            isFirstTick <- isEmptyMVar mlastTick
            if isFirstTick
                then do
                    putMVar mlastTick currentTime
                    render currentTime initTime opts completed
                else do
                    lastTick <- swapMVar mlastTick currentTime
                    render lastTick initTime opts completed
        handleMessage Nothing = handleMessage (Just 1)

render :: UTCTime -> UTCTime -> Options -> Int -> IO ()
render lastTick initTime opts completed = do
    currentTime <- getCurrentTime
    let progressStr = getProgressStr lastTick currentTime initTime opts completed
    clearLine
    setCursorColumn 0
    putStr progressStr

getProgressStr :: UTCTime -> UTCTime -> UTCTime -> Options -> Int -> String
getProgressStr lastTick currentTime initTime opts completed =
    let fmt = pgFormat opts
        elapsed = diffUTCTime currentTime initTime
        percent = floor (100 * (fromIntegral completed :: Double) /
                         fromIntegral (pgTotal opts)) :: Int
        eta = getEta completed (pgTotal opts) lastTick currentTime
        -- Use TJ Holowaychuk's approach
        tmpStr = replaceMany [ (":elapsed", printf "%3.1f"
                                            (realToFrac elapsed :: Double))
                             , (":current", printf "%3d" completed)
                             , (":total", printf "%3d" (pgTotal opts))
                             , (":percent", printf "%3d%%" percent)
                             , (":eta", printf "%3.1f" eta)
                             ]
                             fmt
        barWidth = pgWidth opts - length (replace ":bar" "" tmpStr)
        barStr = getBarStr (pgCompletedChar opts)
                           (pgPendingChar opts)
                           barWidth
                           percentCompleted
      in replace ":bar" barStr tmpStr
  where
    replaceMany pairs target = foldr (\(old, new) m -> replace old new m)
                                     target
                                     pairs
    percentCompleted = (fromIntegral completed :: Double) /
                       fromIntegral (pgTotal opts)

getEta :: Integral a => a -> a -> UTCTime -> UTCTime -> Double
getEta completed total lastTick currentTime = remaining / secondsPerTick
  where
    secondsSinceLastTick = realToFrac (diffUTCTime currentTime lastTick)
    secondsPerTick = fromIntegral total * secondsSinceLastTick
    remaining = fromIntegral (total - completed)

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
    bcompleted = ceiling $ fromIntegral width * percentCompleted
    bremaining = floor $ fromIntegral width * percentRemaining

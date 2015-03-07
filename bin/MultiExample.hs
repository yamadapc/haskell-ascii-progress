import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (wait)
import Control.Monad (unless)
import System.Console.AsciiProgress (ProgressBar(..), Options(..), isComplete,
                                     def, newProgressBar, tick)

main :: IO ()
main = do
    pg1 <- newProgressBar def { pgWidth = 100 }
    _ <- forkIO $ loop pg1 (100 * 1000)
    threadDelay 1000
    pg2 <- newProgressBar def { pgWidth = 100 }
    _ <- forkIO $ loop pg2 (400 * 1000)
    threadDelay 2000
    pg3 <- newProgressBar def { pgWidth = 100 }
    _ <- forkIO $ loop pg3 (200 * 1000)
    wait $ pgFuture pg1
    wait $ pgFuture pg2
    wait $ pgFuture pg3
  where
    loop pg ts = do
        b <- isComplete pg
        unless b $ do
            threadDelay ts
            tick pg
            loop pg ts

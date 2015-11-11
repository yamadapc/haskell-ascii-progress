import           Control.Concurrent           (threadDelay)
import           Control.Monad                (unless)
import           System.Console.AsciiProgress

main :: IO ()
main = displayConsoleRegions $ do
    pg <- newProgressBar def { pgWidth = 100
                             , pgPendingChar = '░'
                             , pgCompletedChar = '█'
                             , pgFormat = "Working ╢:bar╟"
                             }
    loop pg
  where
    loop pg = do
        b <- isComplete pg
        unless b $ do
            threadDelay $ 200 * 1000
            tick pg
            loop pg

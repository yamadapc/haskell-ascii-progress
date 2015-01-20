import Control.Concurrent
import Control.Monad
import System.Console.AsciiProgress

main :: IO ()
main = do
    pg <- newProgressBar def { pgWidth = 50 }
    loop pg
  where
    loop pg = do
        b <- complete pg
        unless b $ do
            threadDelay $ 200 * 1000
            tick pg
            loop pg

import           Control.Concurrent           (forkIO, threadDelay)
import           Control.Concurrent.Async     (wait)
import           Control.Monad                (forM, unless)
import           System.Console.AsciiProgress
import           System.Random                (randomRIO)

main :: IO ()
main = displayConsoleRegions $ do
    fs <- forM ([0..100] :: [Int]) $ \i -> do
        pg <- newProgressBar def { pgWidth = 100
                              , pgOnCompletion = Just $ "pg " ++ show i ++ " is Done!"
                              }
        speed <- (* 1000) <$> randomRIO (3, 10)
        _ <- forkIO $ loop pg speed
        return $ pgFuture pg
    waitAll fs
  where
    waitAll [] = return ()
    waitAll (f:fs) = wait f >> waitAll fs
    loop pg ts = do
        b <- isComplete pg
        unless b $ do
            threadDelay ts
            tick pg
            loop pg ts

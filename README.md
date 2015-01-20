ascii-progress
==============
- - -
A simple Haskell progress bar for the console. Heavily borrows from TJ
Holowaychuk's Node.JS project [progress](https://github.com/tj/node-progress).

## Usage
```haskell
import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import System.Console.AsciiProgress (Options(..), complete, def,
                                     newProgressBar, tick)

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
```

## License
This code is licensed under the GPLv2 license for Pedro Tacla Yamada. For more
information please refer to the [LICENSE](/LICENSE) file.

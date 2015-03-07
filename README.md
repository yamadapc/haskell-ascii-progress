ascii-progress
==============
[![hackage version](https://img.shields.io/hackage/v/ascii-progress.svg)](http://hackage.haskell.org/package/ascii-progress)
- - -
A simple Haskell progress bar for the console. Heavily borrows from TJ
Holowaychuk's Node.JS project [progress](https://github.com/tj/node-progress).

![demo](/demo.gif)

## Installing
This package is published to hackage as
[`ascii-progress`](http://hackage.haskell.org/package/ascii-progress), so you
can install it with:

```
cabal install ascii-progress
```

## Usage
```haskell
import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import System.Console.AsciiProgress (Options(..), isComplete, def,
                                     newProgressBar, tick)

main :: IO ()
main = do
    pg <- newProgressBar def { pgWidth = 50 }
    loop pg
  where
    loop pg = do
        b <- isComplete pg
        unless b $ do
            threadDelay $ 200 * 1000
            tick pg
            loop pg
```

## License
This code is licensed under the GPLv2 license for Pedro Tacla Yamada. For more
information please refer to the [LICENSE](/LICENSE) file.

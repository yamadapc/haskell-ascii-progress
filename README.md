ascii-progress
==============
[![hackage version](https://img.shields.io/hackage/v/ascii-progress.svg)](http://hackage.haskell.org/package/ascii-progress)
[![Build Status](https://travis-ci.org/yamadapc/haskell-ascii-progress.svg?branch=master)](https://travis-ci.org/yamadapc/haskell-ascii-progress)
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

## Multiple progress bar support
Though still rudimentary, there's support for running multiple concurrent
progress bars. The [`multi-example`](/bin/MultiExample.hs) shows off this
feature:
![demo-multi](/demo-multi.gif)

## Examples
The [bin](/bin) directory contains the example above and a more complex example
which uses `http-conduit` to download an image from imgur, printing the
progress using the package.

To build the examples, just configure and compile with `-fexamples`:
```
git clone https://github.com/yamadapc/haskell-ascii-progress
cabal install -j --only-dep -fexamples
cabal configure -fexamples
cabal run example
# ...
cabal run download-example
```

## Full documentation
For the complete and updated documentation, please visit our hackage page:

https://hackage.haskell.org/package/ascii-progress/docs/System-Console-AsciiProgress.html

## License
This code is licensed under the MIT license for Pedro Tacla Yamada. For more
information please refer to the [LICENSE](/LICENSE) file.

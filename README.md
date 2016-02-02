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
import System.Console.AsciiProgress (Options(..), displayConsoleRegions,
                                     isComplete, def, newProgressBar, tick)

main :: IO ()
main = displayConsoleRegions $ do
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

## Options
`ascii-progress` uses an `Options` data type for configuring the progress bar.
Available options are:
### `pgFormat :: String`
A format `String` for the progress bar. The following placeholders are
supported:
- `":eta"` (ETA displayed in seconds)
- `":current"` (current tick)
- `":total"` (total number of ticks)
- `":percent"` (percentage completed)
- `":elapsed"` (elapsed time in seconds)
- `":bar"` (the actual progress bar)

#### Example
```haskell
main = do
    pg <- newProgressBar def { pgFormat = ":current/:total [:bar]" }
    -- ...
```

### `pgCompletedChar :: Char`
The character used on the completed part of the bar

There's an example which mimicks the NPM3 progress-bar. It looks like:
```
Working ╢████████████████████████████████████░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░╟
```

### `pgPendingChar :: Char`
The character used on the completed part of the bar

### `pgTotal :: Int`
The total amount of ticks for the bar to be completed

### `pgWidth :: Int`
The progress bar's total width in columns

### `pgOnCompletion :: Maybe String`
What to output when the progress bar is done. The same format placeholders used
in `pgFormat` may be used.

### `pgGetProgressStr :: Options -> Stats -> String`
If all else fails, you can provide a function which determines how a
progress-bar is rendered.

## License
This code is licensed under the MIT license for Pedro Tacla Yamada. For more
information please refer to the [LICENSE](/LICENSE) file.

## Donations
Would you like to buy me a beer? Send bitcoin to 3JjxJydvoJjTrhLL86LGMc8cNB16pTAF3y

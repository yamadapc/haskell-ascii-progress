module System.Console.AsciiProgressSpec where

import System.Console.AsciiProgress
import Data.Time.Clock
import Test.Hspec

spec :: Spec
spec =
    describe "getProgressStr :: Options -> Int -> String" $ do
        it "fills the completed percentage of the total width" $ do
            initTime <- getCurrentTime
            currentTime <- getCurrentTime
            lastTick <- getCurrentTime
            let opts = def { pgTotal = 100
                           , pgWidth = 100
                           , pgFormat = ":bar"
                           }
            getProgressStr lastTick currentTime initTime opts 10 `shouldBe`
                "==========                                                                                          "

        it "replaces `:bar` in the `pgFormat` format `String`" $ do
            initTime <- getCurrentTime
            currentTime <- getCurrentTime
            lastTick <- getCurrentTime
            let opts = def { pgTotal = 100
                           , pgWidth = 100
                           , pgFormat = "working [:bar]"
                           }
            getProgressStr lastTick currentTime initTime opts 10 `shouldBe`
                "working [=========                                                                                 ]"

        it "uses the options' completed and pending characters" $ do
            initTime <- getCurrentTime
            currentTime <- getCurrentTime
            lastTick <- getCurrentTime
            let opts = def { pgTotal = 100
                           , pgWidth = 100
                           , pgCompletedChar = '*'
                           , pgPendingChar = '-'
                           , pgFormat = ":bar"
                           }
            getProgressStr lastTick currentTime initTime opts 10 `shouldBe`
                "**********------------------------------------------------------------------------------------------"

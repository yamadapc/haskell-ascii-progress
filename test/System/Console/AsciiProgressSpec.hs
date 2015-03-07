module System.Console.AsciiProgressSpec where

import System.Console.AsciiProgress
import System.Console.AsciiProgress.Internal
import Test.Hspec
import Test.QuickCheck

mockStats :: Stats
mockStats = Stats { stTotal = 100
                  , stCompleted = 10
                  , stRemaining = 90
                  , stElapsed = 10.0
                  , stPercent = 0.1
                  , stEta = 90.0
                  }


spec :: Spec
spec = do
    describe "getProgressStr :: Options -> Int -> String" $ do
        it "fills the completed percentage of the total width" $ do
            let opts = def { pgTotal = 100
                           , pgWidth = 100
                           , pgFormat = ":bar"
                           }
            getProgressStr opts mockStats `shouldBe`
                "==========                                                                                          "

        it "replaces `:bar` in the `pgFormat` format `String`" $ do
            let opts = def { pgTotal = 100
                           , pgWidth = 100
                           , pgFormat = "working [:bar]"
                           }
            getProgressStr opts mockStats `shouldBe`
                "working [=========                                                                                 ]"

        it "uses the options' completed and pending characters" $ do
            let opts = def { pgTotal = 100
                           , pgWidth = 100
                           , pgCompletedChar = '*'
                           , pgPendingChar = '-'
                           , pgFormat = ":bar"
                           }
            getProgressStr opts mockStats `shouldBe`
                "**********------------------------------------------------------------------------------------------"

    describe "getBar :: Char -> Char -> Int -> Double -> String" $
        it "never overflows the defined max width" $
            forAll (choose (0, 200)) $ \i ->
                forAll (choose (0, 1)) $ \d ->
                    length (getBar '*' '-' i d) == i


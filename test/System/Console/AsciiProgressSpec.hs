module System.Console.AsciiProgressSpec where

import System.Console.AsciiProgress
import Test.Hspec

spec :: Spec
spec =
    describe "getProgressStr :: Options -> Int -> String" $ do
        it "fills the completed percentage of the total width" $ do
            let opts = def { pgTotal = 100
                           , pgWidth = 100
                           , pgFormat = ":bar"
                           }
            getProgressStr opts 10 `shouldBe`
                "==========                                                                                          "

        it "replaces `:bar` in the `pgFormat` format `String`" $ do
            let opts = def { pgTotal = 100
                           , pgWidth = 100
                           , pgFormat = "working [:bar]"
                           }
            getProgressStr opts 10 `shouldBe`
                "working [=========                                                                                 ]"

        it "uses the options' completed and pending characters" $ do
            let opts = def { pgTotal = 100
                           , pgWidth = 100
                           , pgCompletedChar = '*'
                           , pgPendingChar = '-'
                           , pgFormat = ":bar"
                           }
            getProgressStr opts 10 `shouldBe`
                "**********------------------------------------------------------------------------------------------"

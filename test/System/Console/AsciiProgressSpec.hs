{-# LANGUAGE OverloadedStrings #-}
module System.Console.AsciiProgressSpec where

import Test.Hspec
import Test.QuickCheck

import System.Console.AsciiProgress
import System.Console.AsciiProgress.Internal

mockStats :: Stats
mockStats = Stats { stTotal = 100
                  , stCurrent = 10
                  , stRemaining = 90
                  , stElapsed = 10.0
                  , stPercent = 0.1
                  , stEta = 90.0
                  }


spec :: Spec
spec = do
    describe "getProgressTxt :: Options -> Integer -> Text" $ do
        it "fills the completed percentage of the total width" $ do
            let opts = def { pgTotal = 100
                           , pgWidth = 100
                           , pgFormat = Left bar
                           }
            getProgressTxt opts mockStats `shouldBe`
                "==========                                                                                          "

        it "replaces `:bar` in the `pgFormat` format `Text`" $ do
            let opts = def { pgTotal = 100
                           , pgWidth = 100
                           , pgFormat = Right $ "working [:bar]"
                           }
            getProgressTxt opts mockStats `shouldBe`
                "working [=========                                                                                 ]"

        it "uses the options' completed and pending characters" $ do
            let opts = def { pgTotal = 100
                           , pgWidth = 100
                           , pgCompletedChar = '*'
                           , pgPendingChar = '-'
                           , pgFormat = Right ":bar"
                           }
            getProgressTxt opts mockStats `shouldBe`
                "**********------------------------------------------------------------------------------------------"

    describe "getBar :: Char -> Char -> Integer -> Double -> String" $
        it "never overflows the defined max width" $
            forAll (choose (0, 200)) $ \i ->
                forAll (choose (0, 1)) $ \d ->
                    length (getBar '*' '-' i d) == fromInteger i

{-# LANGUAGE OverloadedStrings #-}
module System.Console.AsciiProgressSpec where

import Formatting
import Test.Hspec
import Test.QuickCheck

import System.Console.AsciiProgress
import System.Console.AsciiProgress.Internal
import System.Console.AsciiProgress.Formatting

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
                           , pgFormat = Left $ "working [" % bar % "]"
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

    describe "humanFilesize" $ do
        it "formats memory sizes properly (bytes)" $
           humanFilesize 12 `shouldBe` "12B"

        it "formats memory sizes properly (KB)" $
           humanFilesize 1234 `shouldBe` "1.2KB"

        it "formats memory sizes properly (MB)" $
           humanFilesize 123456789 `shouldBe` "117.7MB"

        it "formats memory sizes properly (GB)" $
           humanFilesize 123456789000 `shouldBe` "115.0GB"

        it "formats memory sizes properly (TB)" $
           humanFilesize 12345678900000 `shouldBe` "11.2TB"

        it "formats memory sizes properly (PB)" $
           humanFilesize 1234567890000000 `shouldBe` "1.1PB"

        it "formats memory sizes properly (beyond)" $
           humanFilesize 1234567890000000000 `shouldBe` "1096.5PB"

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Halytics.Monitor
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@=?))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [lastFive, periodOfThree]

lastFive :: TestTree
lastFive = testCase "Max &^ Last 5" $ Just 5 @=? res
  where
    monitor = g' :: Monitor ('Si (Max &^ Last 5))
    monitor' = foldl notify monitor entries
    entries = [32.0, 45, 33, 1, 2, 3, 4, 5] :: [Double]
    res = result monitor' :: Maybe Double

periodOfThree :: TestTree
periodOfThree = testCase "Max &^ PeriodOf 3" $ Just <$> [45, 3, 5] @=? res
  where
    monitor = g' :: Monitor ('Si (Max &^ PeriodOf 3))
    monitor' = foldl notify monitor entries
    entries = [32.0, 45, 33, 1, 2, 3, 4, 5] :: [Double]
    res = result monitor' :: [Maybe Double]

-- Test Monitor instantiation

type TestA = 'Si Max
type TestB = 'M '[ 'Si Max]
type TestC = 'M '[ 'Si Max, 'Si Max]
type TestD = 'M '[TestB]
type TestE = 'M '[TestC]
type TestF = 'M '[TestE, TestC]

_testA = g' :: Monitor TestA
_testB = g' :: Monitor TestB
_testC = g' :: Monitor TestC
_testD = g' :: Monitor TestD
_testE = g' :: Monitor TestE
_testF = g' :: Monitor TestF

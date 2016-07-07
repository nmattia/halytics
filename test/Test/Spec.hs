{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Control.Lens
import Halytics.Metric
import Halytics.Monitor
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@=?))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [lastFive, periodOfThree, lens1]

lastFive :: TestTree
lastFive = testCase "Max |^ Last 5" $ Just 5 @=? res
  where
    monitor = generate :: Monitor (Max |^ Last 5)
    monitor' = foldl notify monitor entries
    entries = [32.0, 45, 33, 1, 2, 3, 4, 5] :: [Double]
    res = result monitor' :: Maybe Double

periodOfThree :: TestTree
periodOfThree = testCase "Max |^ PeriodOf 3" $ Just <$> [45, 3, 5] @=? res
  where
    monitor = generate :: Monitor (Max |^ PeriodOf 3)
    monitor' = foldl notify monitor entries
    entries = [32.0, 45, 33, 1, 2, 3, 4, 5] :: [Double]
    res = result monitor' :: [Maybe Double]

--

lens1 :: TestTree
lens1 = testCase "_1" $ Just 100 @=? res
  where
    monitor = generate :: Monitor TestC
    monitor' = foldl notify monitor entries
    monitor'' = monitor' & _1 %~ (`notify` 100)
    entries = [32.0, 45, 33, 1, 2, 3, 4, 5] :: [Double]
    res = monitor'' ^. _1 & result :: Maybe Double


-- Test Monitor generation

type TestA = Max
type TestC = (Max, Max)
type TestE = TestC
type TestF = (TestE, TestC)

_testA = generate :: Monitor TestA
_testE = generate :: Monitor TestE
_testF = generate :: Monitor TestF

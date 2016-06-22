{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Control.Lens
import Halytics.Monitor
import Halytics.Metric
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@=?))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [lastFive, periodOfThree, lens1]

lastFive :: TestTree
lastFive = testCase "Max |^ Last 5" $ Just 5 @=? res
  where
    monitor = g' :: Monitor ('L (Max |^ Last 5))
    monitor' = foldl notify monitor entries
    entries = [32.0, 45, 33, 1, 2, 3, 4, 5] :: [Double]
    res = result monitor' :: Maybe Double

periodOfThree :: TestTree
periodOfThree = testCase "Max |^ PeriodOf 3" $ Just <$> [45, 3, 5] @=? res
  where
    monitor = g' :: Monitor ('L (Max |^ PeriodOf 3))
    monitor' = foldl notify monitor entries
    entries = [32.0, 45, 33, 1, 2, 3, 4, 5] :: [Double]
    res = result monitor' :: [Maybe Double]

--

lens1 :: TestTree
lens1 = testCase "_1" $ Just 100 @=? res
  where
    monitor = g' :: Monitor TestB
    monitor' = foldl notify monitor entries
    monitor'' = monitor' & _1 %~ (`notify` 100)
    entries = [32.0, 45, 33, 1, 2, 3, 4, 5] :: [Double]
    res = monitor'' ^. _1 & result :: Maybe Double


-- Test Monitor instantiation

type TestA = 'L Max
type TestB = 'N '[ 'L Max]
type TestC = 'N '[ 'L Max, 'L Max]
type TestD = 'N '[TestB]
type TestE = 'N '[TestC]
type TestF = 'N '[TestE, TestC]

_testA = g' :: Monitor TestA
_testB = g' :: Monitor TestB
_testC = g' :: Monitor TestC
_testD = g' :: Monitor TestD
_testE = g' :: Monitor TestE
_testF = g' :: Monitor TestF

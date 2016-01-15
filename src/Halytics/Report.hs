module Halytics.Report where

import Statistics.Sample (Sample)

import qualified Statistics.Sample as Stats
import qualified Statistics.Quantile as Quant

mean :: Sample -> String
mean sample = "μ : " ++ show (Stats.mean sample)

meanVariance :: Sample -> String
meanVariance sample = "μ : " ++ show μ ++ "\nσ : " ++ show σ
  where
    (μ, var) = Stats.meanVariance sample
    σ = sqrt var

median :: Sample -> String
median sample = "median : " ++ show (Quant.weightedAvg 1 2 sample)

perc95 :: Sample -> String
perc95 sample = "95th percentile : " ++ show (Quant.weightedAvg 95 100 sample)

perc99 :: Sample -> String
perc99 sample = "99th percentile : " ++ show (Quant.weightedAvg 99 100 sample)

report :: [Sample -> String] -> [Sample] -> IO ()
report fs = mapM_ (\s -> putStrLn . unlines $ map (\f -> f s) fs)

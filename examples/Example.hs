{-# LANGUAGE RankNTypes #-}

import Control.Monad (replicateM_, void, foldM)
import Server        (Server(..), slowSum, fastSum, request)
import Statistics.Sample (Sample)
import System.Random (randomRIO)

import qualified Halytics.Collector as HC
import qualified Statistics.Sample as Stats
import qualified Statistics.Quantile as Quant
import qualified System.Clock  as Clk

type ServerID = Int

main :: IO ()
main = do
  c <- foldM (\c _ -> performARequest c) HC.empty [1 .. numberOfRequests]
  let sample = HC.toSamples c [(==) 1, (==) 2, (==) 3]
  mapM_ reportMetrics sample
  return ()
  where
    numberOfRequests = 10000

reportMetrics :: Sample -> IO ()
reportMetrics sample = do
  putStrLn "---"
  putStrLn $ "μ : " ++ show μ
  putStrLn $ "σ : " ++ show σ
  putStrLn $ "median : " ++ show median
  putStrLn $ "95th percentile : " ++ show perc95
  putStrLn $ "99th percentile : " ++ show perc99
  putStrLn "---"
  where
    (μ, var) = Stats.meanVariance sample
    σ = sqrt var
    median = Quant.weightedAvg 1 2 sample
    perc95 = Quant.weightedAvg 95 100 sample
    perc99 = Quant.weightedAvg 99 100 sample

performARequest :: HC.Collector ServerID -> IO (HC.Collector ServerID)
performARequest collector = do
  (serverID, server) <- (servers !!) <$> randomRIO (0, numberOfServers - 1)
  input <- randomRIO (1000, 100000)
  (_result, us) <- measure $ request server input
  return $ HC.notify collector serverID us
  where
    numberOfServers = length servers

servers :: [(ServerID, Server)]
servers = zip [1 .. ] [slowServer, fastServer, slowServer]
  where fastServer = Server fastSum
        slowServer = Server slowSum

timeSpecAsMicroSecs :: Clk.TimeSpec -> Double
timeSpecAsMicroSecs t = fromIntegral (Clk.timeSpecAsNanoSecs t)  / 1e3

measure :: forall a . IO a -> IO (a, Double)
measure f = do
  was <- Clk.getTime Clk.Monotonic
  res <- f
  is <- Clk.getTime Clk.Monotonic
  return (res, timeSpecAsMicroSecs $ Clk.diffTimeSpec was is)

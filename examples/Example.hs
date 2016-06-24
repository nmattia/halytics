{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

import           Control.Lens
import           Control.Monad              (foldM, replicateM_, unless, void)
import           Data.Bool                  (bool)
import           Data.Proxy
import           Halytics.Metric
import           Halytics.Metric.Statistics
import           Halytics.Monitor
import           Halytics.Time
import           Network.Wreq               (get)
import           Server                     (Server (..), fastSum, request,
                                             slowSum)
import           Statistics.Sample          (Sample)
import           System.Environment         (getArgs)
import           System.Random              (randomRIO)

import qualified Statistics.Quantile        as Quant
import qualified Statistics.Sample          as Stats

type ServerID = Int

main :: IO ()
main = getArgs >>= mapM_ (\case
  "max" -> simpleMax
  "stats" -> stats 10 "http://haskell.org"
  "stats-str" -> stats' 10 "http://haskell.org"
  "sla" -> sla
  str -> putStrLn $ "Unknown: " ++ str)

--------------------------------------------------------------------------------
-- Basic usage

simpleMax :: IO ()
simpleMax =
  let m = notifyMany (generate :: Monitor ('L Max)) [1, 42.0, 341, 3.1415]
  in putStrLn $ result m

--------------------------------------------------------------------------------
-- Statistics

type SomeMetrics =
  (N '[ L Median
      , L (Percentile 95)
      , L (Percentile 99)
      , L Max ])

-- |
--
stats :: Int -> String -> IO ()
stats n url = do
    m <- monitor n url
    m^._1&result & (\(med :: Double) -> putStrLn $ "Median: " ++ show med)
    m^._2&result & (\(p95 :: Double) -> putStrLn $ "P95: " ++ show p95)
    m^._3&result & (\(p99 :: Double) -> putStrLn $ "P99: " ++ show p99)
    m^._4&result & (\(mx :: Maybe Double) -> putStrLn $ "Max: " ++ show mx)


-- |
--
stats' :: Int -> String -> IO ()
stats' n url = do
    m <- monitor n url
    putStrLn $ m^._1&result
    putStrLn $ m^._2&result
    putStrLn $ m^._3&result
    putStrLn $ m^._4&result

monitor :: Int -> String -> IO (Monitor SomeMetrics)
monitor n url = go n generate
  where go 0 m = return m
        go n m = notify m <$> timeIO_ (get url) >>= go (n-1)
        timeIO_ io = snd <$> timeIO io

--------------------------------------------------------------------------------
-- SLA

data MySLA = MySLA Double

instance Initialize MySLA where
  initialize (MySLA x) = (x, Nothing)

instance Resultable MySLA Bool where
  r _ (x, xs) = maybe False (x >=) res
    where
      res = r (Proxy :: Proxy Max) xs :: Maybe Double

instance Collect MySLA where
  type S MySLA = (Double, Maybe Double)
  collect _ (m, xs) x = (m, collect (Proxy :: Proxy Max) xs x)

type Benchmarker =
  (N '[ L Max
      , L (Max |^ PeriodOf 6)
      , L (Max |^ Every 6)
      , N '[ L Max
           , L (Max |^ Last 10)]
      , L MySLA ])

sla :: IO ()
sla = do
    m <- foldM (\mo _ -> notify mo <$> performARequest) m0 [1.. 1000]
    m^._1 & (putStrLn . result)
    m^._4._2 & (putStrLn . result)
    m^._5 & (putStrLn . bool "Failed :(" "Passed!" . result)
  where
    m0 = generate&_5 %<~ MySLA 100.0 :: Monitor Benchmarker

-- Time measuring

performARequest :: IO Double
performARequest = do
  input <- randomRIO (1000, 100000)
  (_result, us) <- timeIO $ request (Server slowSum) input
  return us

servers :: [(ServerID, Server)]
servers = zip [1 .. ] [slowServer, fastServer, slowServer]
  where fastServer = Server fastSum
        slowServer = Server slowSum

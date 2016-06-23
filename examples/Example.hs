{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

import           Control.Lens
import           Control.Monad       (foldM, replicateM_, unless, void)
import           Data.Proxy
import           Halytics.Metric
import           Halytics.Metric.Statistics
import           Halytics.Monitor
import           Halytics.Time
import           Network.Wreq        (get)
import           Server              (Server (..), fastSum, request, slowSum)
import           Statistics.Sample   (Sample)
import           System.Environment  (getArgs)
import           System.Random       (randomRIO)

import qualified Statistics.Quantile as Quant
import qualified Statistics.Sample   as Stats

type ServerID = Int

main :: IO ()
main = getArgs >>= mapM_ (\case
  "max" -> simpleMax
  "stats" -> stats 4 ""
  "sla" -> sla
  str -> putStrLn $ "Unknown: " ++ str)

-------------------------------------------------------------------------------
-- Basic usage

simpleMax :: IO ()
simpleMax =
  let m' = collectManyFor (generate :: Monitor ('L Max)) [1, 42.0, 341, 3.1415]
  in putStrLn $ result m'


-------------------------------------------------------------------------------
-- Statistics

type Stats =
  (N '[ L Median
      , L (Percentile 95)
      , L (Percentile 99)
      , L Max ])

stats :: Int -> String -> IO ()
stats n url = replicateM_ n $ timeIO (get url)

monitor :: IO (Monitor Stats)
monitor = go generate 100
  where go m 0 = return m
        go m n = do {m' <- (notify m . snd) <$> timeIO (get "http://google.com"); go m' (n-1)}


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
    ms <- foldM (\mo _ -> notify mo <$> performARequest) m0 [1.. 1000]
    ms^._1 & (putStrLn . result)
    ms^._4._2 & (putStrLn . result)
    ms^._5 & (putStrLn . (\ok -> if ok then "Passed!" else "Failed :(") . result)
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

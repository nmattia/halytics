{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

import           Control.Monad       (foldM, replicateM_, void)
import           Server              (Server (..), fastSum, request, slowSum)
import Data.Proxy
import           Statistics.Sample   (Sample)
import           System.Random       (randomRIO)

import qualified Statistics.Quantile as Quant
import qualified Statistics.Sample   as Stats
import qualified System.Clock        as Clk

import Halytics.Monitor

type ServerID = Int

type Benchmarker =
  (M '[ Si Max
      , Si (Percentile 95)
      , Si (Max &^ PeriodOf 6)
      , Si (Max &^ Every 6)
      , M '[ Si Max
           , Si (Max &^ Last 10)]])

main :: IO ()
main = flop
  where
    numberOfRequests = 10000

flop :: IO ()
flop = do
    ms' <- foldM (\mo _ -> notify mo <$> performARequest) m0 [1.. 1000]
    putStrLn $ result $ n1 ms'
    putStrLn $ result $ n2 ms'
    mapM_ putStrLn (take 5 $ result $ n3 ms' :: [String])
  where
    m0 = g' :: Monitor Benchmarker

-- Time measuring

performARequest :: IO Double
performARequest = do
  input <- randomRIO (1000, 100000)
  (_result, us) <- measure $ request (Server slowSum) input
  return us

servers :: [(ServerID, Server)]
servers = zip [1 .. ] [slowServer, fastServer, slowServer]
  where fastServer = Server fastSum
        slowServer = Server slowSum

timeSpecAsMicroSecs :: Clk.TimeSpec -> Double
timeSpecAsMicroSecs t = fromIntegral (Clk.timeSpecAsNanoSecs t)  / 1e3

measure :: IO a -> IO (a, Double)
measure f = do
  was <- Clk.getTime Clk.Monotonic
  res <- f
  is <- Clk.getTime Clk.Monotonic
  return (res, timeSpecAsMicroSecs $ Clk.diffTimeSpec was is)

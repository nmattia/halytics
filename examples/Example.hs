{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

import           Control.Monad       (foldM, replicateM_, void)
import           Server              (Server (..), fastSum, request, slowSum)
import Data.Proxy
import           Statistics.Sample   (Sample)
import           System.Random       (randomRIO)

import qualified Halytics.Monitor    as HM
import qualified Halytics.Report     as HR
import qualified Statistics.Quantile as Quant
import qualified Statistics.Sample   as Stats
import qualified System.Clock        as Clk

import Halytics.Monitor

type ServerID = Int

type Benchmarker = (M '[Max, Percentile 95, Max &^ Every 6])
{-type Be = (M '[Si [],Max])-}

mmm :: Monitor Be
mmm = undefined

main :: IO ()
main = flop
  where
    numberOfRequests = 10000

flop :: IO ()
flop = do
    ms' <- foldM (\mo _ -> HM.notify mo <$> performARequest) m0 [1.. 1000]
    putStrLn $ HM.result $ HM.n1 ms'
    putStrLn $ HM.result $ HM.n2 ms'
    putStrLn $ HM.result $ HM.n3 ms'
  where
    m0 = g' :: HM.Monitor Benchmarker

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

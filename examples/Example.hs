{-# LANGUAGE DataKinds #-}

import           Control.Monad       (foldM, replicateM_, void)
import           Server              (Server (..), fastSum, request, slowSum)
import Data.Proxy
import           Statistics.Sample   (Sample)
import           System.Random       (randomRIO)

import qualified Halytics.Collector  as HC
import qualified Halytics.Monitor    as HM
import qualified Halytics.Report     as HR
import qualified Statistics.Quantile as Quant
import qualified Statistics.Sample   as Stats
import qualified System.Clock        as Clk

type ServerID = Int
{-type Max = (HM.Max, Maybe Double)-}
{-type Min = (HM.Min, Maybe Double)-}

{-type Max' = (HM.Max, String)-}
{-type Min' = (HM.Min, String)-}

{-type Percentile n = (HM.Percentile n, Maybe Double)-}

type Benchmarker = (HM.M '[HM.Max, HM.Percentile 95])

main :: IO ()
main = flop
  where
    numberOfRequests = 10000

flop :: IO ()
flop = do
    ms' <- foldM (\mo _ -> HM.notii mo <$> performARequest') ms [1.. 1000]
    let (m1, Just ms1) = HM.pop' ms'
    putStrLn $ HM.result' p m1
    return ()
  where
    ms = HM.g' :: HM.Monitor' Benchmarker
    p :: Proxy HM.Max
    p = Proxy

performARequest' :: IO Double
performARequest' = do
  input <- randomRIO (1000, 100000)
  (_result, us) <- measure $ request (Server slowSum) input
  return us

performARequest :: HC.Collector ServerID -> IO (HC.Collector ServerID)
performARequest collector = do
  (serverID, server) <- let randomIndex = randomRIO (0, numberOfServers -1)
                        in (servers !!) <$> randomIndex
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

measure :: IO a -> IO (a, Double)
measure f = do
  was <- Clk.getTime Clk.Monotonic
  res <- f
  is <- Clk.getTime Clk.Monotonic
  return (res, timeSpecAsMicroSecs $ Clk.diffTimeSpec was is)

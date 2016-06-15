{-# LANGUAGE DataKinds #-}

import           Control.Monad       (foldM, replicateM_, void)
import           Server              (Server (..), fastSum, request, slowSum)
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

main :: IO ()
main = flop
  where
    numberOfRequests = 10000


flop :: IO ()
flop = do
    m' <- foldM (\mo _ -> HM.update mo <$> performARequest') m [1.. 1000]
    putStrLn $ HM.result m'

    ms' <- foldM (\mo _ -> HM.notify mo <$> performARequest') ms [1.. 1000]
    {-let m1 :< m2 :< m3 = ms'-}
    let (m1, Just ms1) = HM.pop ms'
    let (m2, Just ms2) = HM.pop ms1
    let (m3, _) = HM.pop ms2
    putStrLn $ HM.result m1
    putStrLn $ HM.result m2
    putStrLn $ HM.result m3
    return ()
  where
    m = HM.monitor :: HM.Monitor HM.Max
    ms = HM.generate :: HM.Monitors '[HM.Max, HM.Percentile 99, HM.Last 5]


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

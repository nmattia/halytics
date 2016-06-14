{-# LANGUAGE DataKinds #-}

import           Control.Monad       (foldM, replicateM_, void)
import           Server              (Server (..), fastSum, request, slowSum)
import           Statistics.Sample   (Sample)
import           System.Random       (randomRIO)

import qualified Halytics.Collector  as HC
import qualified Halytics.Monitor    as HM
import qualified Halytics.Monitor2   as HM2
import qualified Halytics.Monitor3   as HM3
import qualified Halytics.Report     as HR
import qualified Statistics.Quantile as Quant
import qualified Statistics.Sample   as Stats
import qualified System.Clock        as Clk

type ServerID = Int
type Max = (HM.Max, Maybe Double)
type Min = (HM.Min, Maybe Double)

type Max' = (HM.Max, String)
type Min' = (HM.Min, String)

type Percentile n = (HM.Percentile n, Maybe Double)

main :: IO ()
main = flop
  where
    numberOfRequests = 10000

stringMonitor :: IO ()
stringMonitor = do
    m' <- foldM (\mo _ -> HM.notify mo <$> performARequest') m [1.. 100]
    print $ HM.toValues m'
  where
    m = HM.generate ::  HM.Monitor '[Min']

flop :: IO ()
flop = do
    m' <- foldM (\mo _ -> HM3.update mo <$> performARequest') m [1.. 1000]
    putStrLn $ HM3.result m'

    ms' <- foldM (\mo _ -> HM3.notify mo <$> performARequest') ms [1.. 1000]
    {-let m1 :< m2 :< m3 = ms'-}
    let (m1, Just ms1) = HM3.pop ms'
    let (m2, Just ms2) = HM3.pop ms1
    let (m3, _) = HM3.pop ms2
    putStrLn $ HM3.result m1
    putStrLn $ HM3.result m2
    putStrLn $ HM3.result m3
    return ()
  where
    m = HM3.monitor :: HM3.Monitor HM3.Max
    ms = HM3.generate :: HM3.Monitors '[HM3.Max, HM3.Percentile 99, HM3.Last 5]

doubleMonitor :: IO ()
doubleMonitor = do
    m' <- foldM (\mo _ -> HM2.notify mo <$> performARequest') m [1.. 100]
    putStrLn $ HM2.toValue m'
  where
    m = HM2.generate ::  HM2.Monitor '[HM2.Percentile 95]

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

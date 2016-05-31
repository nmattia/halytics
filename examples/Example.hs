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

main :: IO ()
main = do
  putStrLn "   --- Old Collector --- "
  c <- foldM (\c _ -> performARequest c) HC.empty [1 .. numberOfRequests]
  let samples = HC.toSamples_ c [(==) 1, (==) 2, (==) 3]
  HR.report [HR.meanVariance, HR.median, HR.perc95, HR.perc99] samples

  putStrLn "   --- Monitor --- "

  let m = HM.generate ::  HM.Monitor '[HM.Max, HM.Min, HM.Percentile 95]
  m' <- foldM (\mo _ -> performARequest' >>= \dt -> return $ HM.notify mo dt) m [1.. 100]
  print $ HM.toValues m'

  let m'' = HM.generate' :: HM.Monitor' '[(HM.Max, String)]
      m''' = HM.notify' m'' 0.5
      m'''' = HM.notify' m''' 12.3

  return ()

  where
    numberOfRequests = 10000

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

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

import           Control.Lens
import           Control.Monad       (foldM, replicateM_, void, unless)
import           Data.Proxy
import           Halytics.Monitor
import           Halytics.Time
import           Server              (Server (..), fastSum, request, slowSum)
import           Statistics.Sample   (Sample)
import           System.Random       (randomRIO)

import qualified Statistics.Quantile as Quant
import qualified Statistics.Sample   as Stats
import qualified System.Clock        as Clk

type ServerID = Int

data MySLA = MySLA Double

instance Init MySLA where
  g'' (MySLA x) = (x, [])

instance Resultable MySLA Bool where
  r _ (x, xs) = maybe False (x >=) res
    where
      res = r (Proxy :: Proxy Max) xs :: Maybe Double

instance Storable MySLA where
  type S MySLA = (Double, [Double])
  u' _ (m, xs) x = (m, u' (Proxy :: Proxy Max) xs x)

type Benchmarker =
  (N '[ L Max
      , L (Max |^ PeriodOf 6)
      , L (Max |^ Every 6)
      , N '[ L Max
           , L (Max |^ Last 10)]])

main :: IO ()
main = do
    sla
    ms <- foldM (\mo _ -> notify mo <$> performARequest) m0 [1.. 1000]
    ms^._1 & (putStrLn . result)
    ms^._4._2 & (putStrLn . result)
  where
    m0 = g' :: Monitor Benchmarker

sla :: IO ()
sla = do
  ms <- foldM (\mo _ -> notify mo <$> performARequest)
              (monitorWith (MySLA 100.0))
              [1.. 1000]
  unless (result ms) $ error "SLA failed"

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

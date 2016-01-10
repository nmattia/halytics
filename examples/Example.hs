{-# LANGUAGE RankNTypes #-}

import Control.Monad (replicateM_, void, foldM_)
import Server        (Server(..), slowSum, fastSum, request)
import System.Random (randomRIO)

import qualified Halytics.Collector as HC
import qualified System.Clock  as Clk

main :: IO ()
main =
  foldM_ (\c _ -> performARequest c) HC.empty [1 .. numberOfRequests]
  where
    numberOfRequests = 1000000

performARequest :: HC.Collector Double -> IO (HC.Collector Double)
performARequest collector = do
  (serverID, server) <- (servers !!) <$> randomRIO (0, numberOfServers - 1)
  input <- randomRIO (1000, 100000)
  (_result, us) <- measure $ request server input
  putStrLn $ "Time was " ++ show us ++ " us"
  return $ HC.notify collector us
  where
    numberOfServers = length servers

servers :: [(Int, Server)]
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

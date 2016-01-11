{-# LANGUAGE RankNTypes #-}

import Control.Monad (replicateM_, void, foldM)
import Server        (Server(..), slowSum, fastSum, request)
import System.Random (randomRIO)

import qualified Halytics.Collector as HC
import qualified System.Clock  as Clk

type ServerID = Int

main :: IO ()
main = do
  c <- foldM (\c _ -> performARequest c) HC.empty [1 .. numberOfRequests]
  let sample = HC.toSample c [(==) 1, (==) 2, (==) 3]
  mapM_ (putStrLn . show . HC.mean) sample
  return ()
  where
    numberOfRequests = 10000

performARequest :: HC.Collector ServerID -> IO (HC.Collector ServerID)
performARequest collector = do
  (serverID, server) <- (servers !!) <$> randomRIO (0, numberOfServers - 1)
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

measure :: forall a . IO a -> IO (a, Double)
measure f = do
  was <- Clk.getTime Clk.Monotonic
  res <- f
  is <- Clk.getTime Clk.Monotonic
  return (res, timeSpecAsMicroSecs $ Clk.diffTimeSpec was is)

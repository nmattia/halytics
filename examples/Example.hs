import Control.Monad (replicateM_, void)
import Server (Server(..), slowSum, fastSum, request)
import System.Random (randomRIO)

main :: IO ()
main = replicateM_ numberOfRequests performARequest

numberOfRequests :: Int
numberOfRequests = 1000000

performARequest :: IO ()
performARequest = do
  (serverID, server) <- (servers !!) <$> randomRIO (0, numberOfServers - 1)
  input <- randomRIO (1, 5000)
  void $ request server input
 where
  numberOfServers = length servers

servers :: [(Int, Server)]
servers = zip [1 .. ] [slowServer, fastServer, slowServer]
  where fastServer = Server fastSum
        slowServer = Server slowSum

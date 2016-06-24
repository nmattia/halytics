module Halytics.Time where

import qualified System.Clock as Clk

timeSpecAsMicroSecs :: Clk.TimeSpec -> Double
timeSpecAsMicroSecs t = fromIntegral (Clk.toNanoSecs t)  / 1e3

timeIO :: IO a -> IO (a, Double)
timeIO f = do
  was <- Clk.getTime Clk.Monotonic
  res <- f
  is <- Clk.getTime Clk.Monotonic
  return (res, timeSpecAsMicroSecs $ Clk.diffTimeSpec was is)

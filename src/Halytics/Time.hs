{-# LANGUAGE CPP #-}

module Halytics.Time where

import qualified System.Clock as Clk

timeSpecAsMicroSecs :: Clk.TimeSpec -> Double
#if MIN_VERSION_clock(0,7,0)
timeSpecAsMicroSecs t = fromIntegral (Clk.toNanoSecs t)  / 1e3
#else
timeSpecAsMicroSecs t = fromIntegral (Clk.timeSpecAsNanoSecs t)  / 1e3
#endif

timeIO :: IO a -> IO (a, Double)
timeIO f = do
  was <- Clk.getTime Clk.Monotonic
  res <- f
  is <- Clk.getTime Clk.Monotonic
  return (res, timeSpecAsMicroSecs $ Clk.diffTimeSpec was is)

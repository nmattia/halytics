module Halytics.Collector
  ( Collector
  , Sample
  , empty
  , mean
  , notify
  , toSamples
  , toSamples_
  ) where

import Control.Monad (foldM_)
import Control.Monad.ST(runST)
import Statistics.Sample (Sample)

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M

data Collector k = Collector [(k, Double)]

empty :: Collector k
empty = Collector []

notify :: Collector k -> k -> Double -> Collector k
notify (Collector es) k v = Collector ((k, v):es)

toSamples :: Collector k
          -> (k -> Bool)
          -> [k -> Bool]
          -> (Collector k, [Sample])
toSamples collector@(Collector es) feedbackPred preds = (collector', samples)
  where
    samples = toSamples_ collector preds
    collector' = Collector $ filter (feedbackPred . fst) es

toSamples_ :: Collector k -> [k -> Bool] -> [Sample]
toSamples_ (Collector es) = map (toValues . (`filterOnKey` es))
  where
    filterOnKey p = filter (p . fst)
    toDoubles = map snd
    toValues = doublesToSample . toDoubles

doublesToSample :: [Double] -> Sample
doublesToSample ds = runST $ do
  vec <- M.new l
  foldM_ (insert vec) (l - 1) ds
  V.freeze vec
  where
    insert vec i v = M.write vec i v  >> return (i-1)
    l = length ds

mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

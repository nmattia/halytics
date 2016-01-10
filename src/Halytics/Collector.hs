module Halytics.Collector
  ( Collector
  , empty
  , notify
  , toSample
  ) where

data Collector k = Collector [(k, Double)]

empty :: Collector k
empty = Collector []

notify :: Collector k -> k -> Double -> Collector k
notify (Collector es) k v = Collector ((k, v):es)

toSample :: Collector k -> [k -> Bool] -> [[Double]]
toSample (Collector es) = map (toValues . (`filterOnKey` es))
  where
    filterOnKey p = filter (p . fst)
    toValues = map snd

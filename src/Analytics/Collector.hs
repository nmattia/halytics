module Analytics.Collector where

data Collector e = Collector
data Sample e = Sample

toSample :: Collector e -> Sample e
toSample = undefined

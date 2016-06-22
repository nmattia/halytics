module Halytics.Metric.Statistics where

import           Data.Proxy
import           GHC.TypeLits
import           Halytics.Monitor.Internal

import qualified Data.Vector.Unboxed as V
import qualified Statistics.Quantile as Stats
import qualified Statistics.Sample   as Stats

newtype StoredStats a = StoredStats a

class FromStats a r where
  func :: Proxy a -> V.Vector Double -> r

instance Collect (StoredStats a) where
  type S (StoredStats a) = [Double]
  collect _ = flip (:)

instance Default (StoredStats a) where
  initial _ = []

instance (Collect a, FromStats a r) => Resultable (StoredStats a) r where
  r _ xs = func (Proxy :: Proxy a) (V.fromList xs)

-- From Statistics.Sample

data Mean
instance FromStats Mean Double where func _ = Stats.mean

data HarmonicMean
instance FromStats HarmonicMean Double where func _ = Stats.harmonicMean

data GeometricMean
instance FromStats GeometricMean Double where func _ = Stats.geometricMean

data CentralMoment :: Nat -> *

instance (KnownNat k) => FromStats (CentralMoment k) Double where
  func _ = Stats.centralMoment k
    where k  = fromInteger $ natVal (Proxy :: Proxy k)

data CentralMoments :: Nat -> Nat -> *

instance (KnownNat k, KnownNat j)
         => FromStats (CentralMoments k j) (Double, Double) where
  func _ = Stats.centralMoments k j
    where
      k  = fromInteger $ natVal (Proxy :: Proxy k)
      j  = fromInteger $ natVal (Proxy :: Proxy j)

data Skewness
instance FromStats Skewness Double where func _ = Stats.skewness

data Kurtosis
instance FromStats Kurtosis Double where func _ = Stats.kurtosis

data Variance
instance FromStats Variance Double where func _ = Stats.variance

data VarianceUnbiased
instance FromStats VarianceUnbiased Double where
  func _ = Stats.varianceUnbiased

data MeanVariance
instance FromStats MeanVariance (Double, Double) where
  func _ = Stats.meanVariance

data MeanVarianceUnb
instance FromStats MeanVarianceUnb (Double, Double) where
  func _ = Stats.meanVarianceUnb

data StdDev
instance FromStats StdDev Double where
  func _ = Stats.stdDev

data FastVariance
instance FromStats FastVariance Double where
  func _ = Stats.fastVariance

data FastVarianceUnbiased
instance FromStats FastVarianceUnbiased Double where
  func _ = Stats.fastVarianceUnbiased

data FastStdDev
instance FromStats FastStdDev Double where
  func _ = Stats.fastStdDev

-- From Statistics.Quantile

-- Helpers

type Quantile k q = WeightedAvg k q
type Percentile k = Quantile k 100
type Median = Percentile 50

data WeightedAvg :: Nat -> Nat -> *
instance (KnownNat k, KnownNat q)
         => FromStats (WeightedAvg k q) Double where
  func _ = Stats.weightedAvg k q
    where
      k  = fromInteger $ natVal (Proxy :: Proxy k)
      q  = fromInteger $ natVal (Proxy :: Proxy q)

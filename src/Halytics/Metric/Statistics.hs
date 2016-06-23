{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}

module Halytics.Metric.Statistics where

import           Data.Proxy
import           GHC.TypeLits
import           Halytics.Monitor.Internal

import qualified Data.Vector.Unboxed       as V
import qualified Statistics.Quantile       as Stats
import qualified Statistics.Sample         as Stats

newtype StoredStats a = StoredStats a

class FromStats a r | a -> r where
  func :: Proxy a -> V.Vector Double -> r

instance Collect (StoredStats a) where
  type S (StoredStats a) = [Double]
  collect _ = flip (:)

instance Default (StoredStats a) where
  initial _ = []

instance (Collect a, FromStats a r) => Resultable (StoredStats a) r where
  r _ xs = func (Proxy :: Proxy a) (V.fromList xs)

-- From Statistics.Sample

data Mean'
type Mean = StoredStats Mean'
instance FromStats Mean Double where func _ = Stats.mean

data HarmonicMean'
type HarmonicMean = StoredStats HarmonicMean'
instance FromStats HarmonicMean Double where func _ = Stats.harmonicMean

data GeometricMean'
type GeometricMean = StoredStats GeometricMean'
instance FromStats GeometricMean Double where func _ = Stats.geometricMean

data CentralMoment' :: Nat -> *
type CentralMoment k = StoredStats (CentralMoment' k)
instance (KnownNat k) => FromStats (CentralMoment k) Double where
  func _ = Stats.centralMoment k
    where k  = fromInteger $ natVal (Proxy :: Proxy k)

data CentralMoments' :: Nat -> Nat -> *
type CentralMoments k j = StoredStats (CentralMoments' k j)
instance (KnownNat k, KnownNat j)
         => FromStats (CentralMoments k j) (Double, Double) where
  func _ = Stats.centralMoments k j
    where
      k  = fromInteger $ natVal (Proxy :: Proxy k)
      j  = fromInteger $ natVal (Proxy :: Proxy j)

data Skewness'
type Skewness = StoredStats Skewness'
instance FromStats Skewness Double where func _ = Stats.skewness

data Kurtosis'
type Kurtosis = StoredStats Kurtosis'
instance FromStats Kurtosis Double where func _ = Stats.kurtosis

data Variance'
type Variance = StoredStats Variance'
instance FromStats Variance Double where func _ = Stats.variance

data VarianceUnbiased'
type VarianceUnbiased = StoredStats VarianceUnbiased'
instance FromStats VarianceUnbiased Double where
  func _ = Stats.varianceUnbiased

data MeanVariance'
type MeanVariance = StoredStats MeanVariance'
instance FromStats MeanVariance (Double, Double) where
  func _ = Stats.meanVariance

data MeanVarianceUnb'
type MeanVarianceUnb = StoredStats MeanVarianceUnb'
instance FromStats MeanVarianceUnb (Double, Double) where
  func _ = Stats.meanVarianceUnb

data StdDev'
type StdDev = StoredStats StdDev'
instance FromStats StdDev Double where
  func _ = Stats.stdDev

data FastVariance'
type FastVariance = StoredStats FastVariance'
instance FromStats FastVariance Double where
  func _ = Stats.fastVariance

data FastVarianceUnbiased'
type FastVarianceUnbiased = StoredStats FastVarianceUnbiased'
instance FromStats FastVarianceUnbiased Double where
  func _ = Stats.fastVarianceUnbiased

data FastStdDev'
type FastStdDev = StoredStats FastStdDev'
instance FromStats FastStdDev Double where
  func _ = Stats.fastStdDev

-- From Statistics.Quantile

-- Helpers

type Quantile k q = WeightedAvg k q
type Percentile k = Quantile k 100
type Median = Percentile 50

data WeightedAvg' :: Nat -> Nat -> *
type WeightedAvg k q = StoredStats (WeightedAvg' k q)
instance (KnownNat k, KnownNat q)
         => FromStats (WeightedAvg k q) Double where
  func _ = Stats.weightedAvg k q
    where
      k  = fromInteger $ natVal (Proxy :: Proxy k)
      q  = fromInteger $ natVal (Proxy :: Proxy q)

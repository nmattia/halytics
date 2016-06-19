{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Halytics.Monitor.Metric.Statistics where

import           Data.Proxy
import           GHC.TypeLits
import           Halytics.Monitor

import qualified Data.Vector.Unboxed as V
import qualified Statistics.Sample   as Stats

newtype StoredStats a = StoredStats a

class FromStats a r where
  func :: Proxy a -> V.Vector Double -> r

instance Storable (StoredStats a) where
  type S (StoredStats a) = [Double]
  u' _ = flip (:)
  g _ = []

instance (Storable a, FromStats a r) => Resultable (StoredStats a) r where
  r _ xs = func (Proxy :: Proxy a) (V.fromList xs)

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

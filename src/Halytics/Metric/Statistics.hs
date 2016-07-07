{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
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
import           Halytics.Monitor.Tuple

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

instance {-# OVERLAPPABLE #-} (Collect (StoredStats a), FromStats a r)
         => Resultable (StoredStats a) r where
  r _ xs = func (Proxy :: Proxy a) (V.fromList xs)


--------------------------------------------------------------------------------
-- From Statistics.Sample

--------------------------------------------------------------------------------
-- Mean

data Mean'
type Mean = StoredStats Mean'
instance FromStats Mean' Double where func _ = Stats.mean

instance Resultable Mean String where
  r _ xs = "Mean: " ++ show res
    where
      res = r (Proxy :: Proxy Mean) xs :: Double

data HarmonicMean'
type HarmonicMean = StoredStats HarmonicMean'
instance FromStats HarmonicMean' Double where func _ = Stats.harmonicMean

instance Resultable HarmonicMean String where
  r _ xs = "Harmonic mean: " ++ show res
    where
      res = r (Proxy :: Proxy HarmonicMean) xs :: Double

data GeometricMean'
type GeometricMean = StoredStats GeometricMean'
instance FromStats GeometricMean' Double where func _ = Stats.geometricMean

instance Resultable GeometricMean String where
  r _ xs = "Geometric mean: " ++ show res
    where
      res = r (Proxy :: Proxy GeometricMean) xs :: Double

--------------------------------------------------------------------------------
-- Central moments

data CentralMoment' :: Nat -> *
type CentralMoment k = StoredStats (CentralMoment' k)
instance (KnownNat k) => FromStats (CentralMoment' k) Double where
  func _ = Stats.centralMoment k
    where k  = fromInteger $ natVal (Proxy :: Proxy k)

instance (KnownNat k) => Resultable (CentralMoment k) String where
  r _ xs = show k ++ "th central moment: " ++ show res
    where
      res = r (Proxy :: Proxy (CentralMoment k)) xs :: Double
      k = natVal (Proxy :: Proxy k) :: Integer

data CentralMoments' :: Nat -> Nat -> *
type CentralMoments k j = StoredStats (CentralMoments' k j)
instance (KnownNat k, KnownNat j)
         => FromStats (CentralMoments' k j) (Double, Double) where
  func _ = Stats.centralMoments k j
    where
      k  = fromInteger $ natVal (Proxy :: Proxy k)
      j  = fromInteger $ natVal (Proxy :: Proxy j)

instance (KnownNat k, KnownNat j)
         => Resultable (CentralMoments k j) (String, String) where
  r _ xs = (kStr, jStr)
    where
      kStr = show k ++ "th central moment: " ++ show kRes
      jStr = show j ++ "th central moment: " ++ show jRes
      kRes = r (Proxy :: Proxy (CentralMoment k)) xs :: Double
      jRes = r (Proxy :: Proxy (CentralMoment j)) xs :: Double
      k = fromInteger $ natVal (Proxy :: Proxy k) :: Integer
      j = fromInteger $ natVal (Proxy :: Proxy j) :: Integer

instance (KnownNat k, KnownNat j)
         => Resultable (CentralMoments k j) String where
  r _ xs = kStr ++ ", " ++ jStr
    where
      (kStr, jStr) = r (Proxy :: Proxy (CentralMoments k j)) xs

--------------------------------------------------------------------------------
-- Curvature and all

data Skewness'
type Skewness = StoredStats Skewness'
instance FromStats Skewness' Double where func _ = Stats.skewness

instance Resultable Skewness String where
  r _ xs = "Skewness: " ++ show res
    where
      res = r (Proxy :: Proxy Skewness) xs :: Double

data Kurtosis'
type Kurtosis = StoredStats Kurtosis'
instance FromStats Kurtosis' Double where func _ = Stats.kurtosis

instance Resultable Kurtosis String where
  r _ xs = "Kurtosis: " ++ show res
    where
      res = r (Proxy :: Proxy Kurtosis) xs :: Double

--------------------------------------------------------------------------------
-- Variance and standard dev

data Variance'
type Variance = StoredStats Variance'
instance FromStats Variance' Double where func _ = Stats.variance

instance Resultable Variance String where
  r _ xs = "Variance: " ++ show res
    where
      res = r (Proxy :: Proxy Variance) xs :: Double

data VarianceUnbiased'
type VarianceUnbiased = StoredStats VarianceUnbiased'
instance FromStats VarianceUnbiased' Double where
  func _ = Stats.varianceUnbiased

instance Resultable VarianceUnbiased String where
  r _ xs = "Unbiased variance: " ++ show res
    where
      res = r (Proxy :: Proxy VarianceUnbiased) xs :: Double

data MeanVariance'
type MeanVariance = StoredStats MeanVariance'
instance FromStats MeanVariance' (Double, Double) where
  func _ = Stats.meanVariance

instance Resultable MeanVariance (String, String) where
  r _ xs = (mStr, vStr)
    where
      mStr = "Mean: " ++ show m
      vStr = "Variance: " ++ show v
      (m, v) = r (Proxy :: Proxy MeanVariance) xs :: (Double, Double)

instance Resultable MeanVariance String where
  r _ xs = mStr ++ ", " ++ vStr
    where
      (mStr, vStr) = r (Proxy :: Proxy MeanVariance) xs :: (String, String)

data MeanVarianceUnb'
type MeanVarianceUnb = StoredStats MeanVarianceUnb'
instance FromStats MeanVarianceUnb' (Double, Double) where
  func _ = Stats.meanVarianceUnb

instance Resultable MeanVarianceUnb (String, String) where
  r _ xs = (mStr, vStr)
    where
      mStr = "Unbiased mean: " ++ show m
      vStr = "Unbiased variance: " ++ show v
      (m, v) = r (Proxy :: Proxy MeanVarianceUnb) xs :: (Double, Double)

instance Resultable MeanVarianceUnb String where
  r _ xs = mStr ++ ", " ++ vStr
    where
      (mStr, vStr) = r (Proxy :: Proxy MeanVarianceUnb) xs :: (String, String)

data StdDev'
type StdDev = StoredStats StdDev'
instance FromStats StdDev' Double where
  func _ = Stats.stdDev

instance Resultable StdDev String where
  r _ xs = "Standard deviation: " ++ show res
    where
      res = r (Proxy :: Proxy StdDev) xs :: Double

data FastVariance'
type FastVariance = StoredStats FastVariance'
instance FromStats FastVariance' Double where
  func _ = Stats.fastVariance

instance Resultable FastVariance String where
  r _ xs = "Fast variance: " ++ show res
    where
      res = r (Proxy :: Proxy FastVariance) xs :: Double

data FastVarianceUnbiased'
type FastVarianceUnbiased = StoredStats FastVarianceUnbiased'
instance FromStats FastVarianceUnbiased' Double where
  func _ = Stats.fastVarianceUnbiased

instance Resultable FastVarianceUnbiased String where
  r _ xs = "Fast unbiased variance: " ++ show res
    where
      res = r (Proxy :: Proxy FastVarianceUnbiased) xs :: Double

data FastStdDev'
type FastStdDev = StoredStats FastStdDev'
instance FromStats FastStdDev' Double where
  func _ = Stats.fastStdDev

instance Resultable FastStdDev String where
  r _ xs = "Fast standard deviation: " ++ show res
    where
      res = r (Proxy :: Proxy FastStdDev) xs :: Double


--------------------------------------------------------------------------------
-- From Statistics.Quantile

-- Helpers

type Quantile k q = WeightedAvg k q
type Percentile k = Quantile k 100
type Median = Percentile 50

data WeightedAvg' :: Nat -> Nat -> *
type WeightedAvg k q = StoredStats (WeightedAvg' k q)
instance (KnownNat k, KnownNat q)
         => FromStats (WeightedAvg' k q) (Maybe Double) where
         -- Here we return a 'Maybe' because 'weightedAvg' throws an exception
         -- on an empty vector
  func _ v = if V.null v then Nothing else Just $ Stats.weightedAvg k q v
    where
      k  = fromInteger $ natVal (Proxy :: Proxy k)
      q  = fromInteger $ natVal (Proxy :: Proxy q)

instance (KnownNat k, KnownNat q) => Resultable (WeightedAvg k q) String where
  r _ xs = str (natVal (Proxy :: Proxy k)) (natVal (Proxy :: Proxy q))
    where
      str 50 100 = "Median: " ++ show res
      str k 100 = show k ++ "th percentile: " ++ show res
      str k q = "Quantile " ++ show k ++ "/" ++ show q ++ ": " ++ show res
      res = r (Proxy :: Proxy (WeightedAvg k q)) xs :: Maybe Double

-- TODO: Add functions that take a 'ContParam'

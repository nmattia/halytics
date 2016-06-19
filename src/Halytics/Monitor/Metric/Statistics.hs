{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Halytics.Monitor.Metric.Statistics where

import           Control.Monad.ST            (runST)
import           Data.Proxy
import           Statistics.Sample           (mean)

import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as MV

import           Halytics.Monitor

newtype StoredStats a = StoredStats a

class FromStats a r where
  func :: Proxy a -> V.Vector Double -> r

instance Storable (StoredStats a) where
  type S (StoredStats a) = [Double]
  u' _ = flip (:)
  g _ = []

instance (Storable a, FromStats a r) => Resultable (StoredStats a) r where
  r _ xs = func (Proxy :: Proxy a) vec
    where
      vec = runST $ do
        vec' <- MV.new l
        mapM_ (uncurry $ MV.write vec') ixed
        V.freeze vec'
      l = length xs
      ixed = zip [0, 1 ..] xs

data Mean

instance FromStats Mean Double where
  func _ = mean

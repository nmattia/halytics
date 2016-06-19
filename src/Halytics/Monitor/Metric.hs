{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Halytics.Monitor.Metric where

import Data.List
import Data.List.Split           (chunksOf)
import Data.Proxy
import GHC.TypeLits
import Safe

import Halytics.Monitor.Internal

data Max

instance Storable Max where
  type S Max = [Double]
  g _ = []
  u' _ xs x = x:xs

instance Resultable Max (Maybe Double) where
  r _ = maximumMay

instance Resultable Max String where
  r _  xs = maybe naught (\x -> "Max: " ++ show x) res
    where
      naught = "No maximum found"
      res = r (Proxy :: Proxy Max) xs :: Maybe Double

data Percentile :: Nat -> *

instance Storable (Percentile n) where
  type S (Percentile n) = [Double]
  u' _ xs x = x:xs
  g _ = []

instance (KnownNat n) => Resultable (Percentile n) (Maybe Double) where
  r _ xs = xs' `atMay` index
    where
      index = l * n `div` 100
      n = fromInteger $ natVal (Proxy :: Proxy n) :: Int
      l = length xs
      xs' = sort xs

instance (KnownNat n) => Resultable (Percentile n) String where
  r _ xs = maybe naught f res
    where
      res = r (Proxy :: Proxy (Percentile n)) xs :: (Maybe Double)
      naught = "No " ++ show n ++ "th percentile available"
      f perc = show n ++ "th percentile: " ++ show perc
      n = fromInteger $ natVal (Proxy :: Proxy n) :: Int

data All :: *

instance Storable All where
  type S All = [Double]
  u' _ xs x = x:xs
  g _ = []

instance Resultable All [Double] where
  r _ xs = xs

data Every :: Nat -> * -> *

instance (Storable s, KnownNat n) => Storable (Every n s) where
  type S (Every n s) = (Integer, S s)
  g _ = (natVal (Proxy :: Proxy n), g (Proxy :: Proxy s))
  u' _ (0, s) x = (natVal (Proxy :: Proxy n), u' (Proxy :: Proxy s) s x)
  u' _ (n, s) _ = (n-1, s)

instance (Resultable t r) => Resultable (Every n t) r where
  r _ (_, s) = r (Proxy :: Proxy t) s

data Last :: Nat -> * -> *

instance (KnownNat n) => Storable (Last n s) where
  type S (Last n s) = [Double]
  g _ = []
  u' _ xs x = take n (x:xs)
    where
      n = fromInteger $ natVal (Proxy :: Proxy n) :: Int

instance (Storable t, Resultable t r) => Resultable (Last n t) r where
  r _ xs = r (Proxy :: Proxy t) s
    where
      s = foldl' (u' (Proxy :: Proxy t)) (g (Proxy :: Proxy t)) xs

data PeriodOf :: Nat -> * -> *

instance Storable (PeriodOf n s) where
  type S (PeriodOf n s) = [Double]
  g _ = []
  u' _ xs x = x:xs

instance (KnownNat n, Resultable t r, Storable t) => Resultable (PeriodOf n t) [r] where
  r _ xs = r (Proxy :: Proxy t) <$> ss
    where
      ss = foldl' (u' (Proxy :: Proxy t)) (g (Proxy :: Proxy t)) <$> xss
      xss = chunksOf n (reverse xs)
      n = fromInteger $ natVal (Proxy :: Proxy n) :: Int

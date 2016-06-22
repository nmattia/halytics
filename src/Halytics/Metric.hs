{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE BangPatterns #-}

module Halytics.Metric where

import Data.List
import Data.List.Split           (chunksOf)
import Data.Proxy
import GHC.TypeLits
import Halytics.Monitor.Internal

data All :: *

instance Storable All where
  type S All = [Double]
  u' _ = flip (:)

instance Default All where
  g _ = []

instance Resultable All [Double] where
  r _ xs = xs

data Max

instance Storable Max where
  type S Max = Maybe Double
  u' _ (Just !x) x' = Just $ max x x'
  u' _ Nothing x' = Just x'

instance Default Max where
  g _ = Nothing

instance Resultable Max (Maybe Double) where
  r _ x = x

instance Resultable Max String where
  r _  xs = maybe naught (\x -> "Max: " ++ show x) res
    where
      naught = "No maximum found"
      res = r (Proxy :: Proxy Max) xs :: Maybe Double

data Min

instance Storable Min where
  type S Min = Maybe Double
  u' _ (Just !x) x' = Just $ min x x'
  u' _ Nothing x' = Just x'

instance Default Min where
  g _ = Nothing

instance Resultable Min (Maybe Double) where
  r _ x = x

instance Resultable Min String where
  r _  xs = maybe naught (\x -> "Min: " ++ show x) res
    where
      naught = "No minimum found"
      res = r (Proxy :: Proxy Min) xs :: Maybe Double

-- Combinators

data Every :: Nat -> * -> *

instance (Storable s, KnownNat n) => Storable (Every n s) where
  type S (Every n s) = (Integer, S s)
  u' _ (0, s) x = (natVal (Proxy :: Proxy n), u' (Proxy :: Proxy s) s x)
  u' _ (n, s) _ = (n-1, s)

instance (KnownNat n, Default s) => Default (Every n s) where
  g _ = (natVal (Proxy :: Proxy n), g (Proxy :: Proxy s))


instance (Resultable t r) => Resultable (Every n t) r where
  r _ (_, s) = r (Proxy :: Proxy t) s

data Last :: Nat -> * -> *

instance (KnownNat n) => Storable (Last n s) where
  type S (Last n s) = [Double]
  u' _ xs x = take n (x:xs)
    where
      n = fromInteger $ natVal (Proxy :: Proxy n) :: Int

instance Default (Last n s) where
  g _ = []

instance (Default t, Storable t, Resultable t r) => Resultable (Last n t) r where
  r _ xs = r (Proxy :: Proxy t) s
    where
      s = foldl' (u' (Proxy :: Proxy t)) (g (Proxy :: Proxy t)) xs

data PeriodOf :: Nat -> * -> *

instance Storable (PeriodOf n s) where
  type S (PeriodOf n s) = [Double]
  u' _ = flip (:)

instance Default (PeriodOf n s) where
  g _ = []

instance (KnownNat n, Resultable t r, Storable t, Default t) => Resultable (PeriodOf n t) [r] where
  r _ xs = r (Proxy :: Proxy t) <$> ss
    where
      ss = foldl' (u' (Proxy :: Proxy t)) (g (Proxy :: Proxy t)) <$> xss
      xss = chunksOf n (reverse xs)
      n = fromInteger $ natVal (Proxy :: Proxy n) :: Int

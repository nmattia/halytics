{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Halytics.Monitor where

import Data.List    (foldl', sort)
import Data.Proxy
import GHC.TypeLits
import Safe

_testA = g' :: Monitor TestA
_testB = g' :: Monitor TestB
_testC = g' :: Monitor TestC
_testD = g' :: Monitor TestD
_testE = g' :: Monitor TestE

type TestA = 'Si Max
type TestB = 'M '[ 'Si Max]
type TestC = 'M '[ 'Si Max, 'Si Max]
type TestD = 'M '[TestB]
type TestE = 'M '[TestC]
type TestF = 'M '[TestE, TestC]

data Tree a = Si a | M [Tree a]

class Generate t where
  g' :: Monitor t

instance (Storable t) => Generate ('Si t) where
  g' = Single (g (Proxy :: Proxy t))

instance {-# OVERLAPPING #-} (Generate t) => Generate ('M '[t]) where
  g' = Multi g'

instance (Generate t, Generate ('M ts)) => Generate ('M (t ': ts)) where
  g' = g' :> g'

class Resultable t r where
  r :: Proxy t -> S t -> r

class Storable t where
  type S t
  u :: Monitor ('Si t) -> Double -> Monitor ('Si t)
  u' :: Proxy t -> S t -> Double -> S t
  g :: Proxy t -> S t
  u (Single s) x = Single $ u' (Proxy :: Proxy t) s x

result' :: (Resultable t r) => Proxy t -> Monitor ('Si t) -> r
result' p (Single s) = r p s

result :: (Resultable t r) => Monitor ('Si t) -> r
result = result' (Proxy :: Proxy t)

n1 :: Monitor ('M (t ': ts)) -> Monitor t
n1 (Multi m) = m
n1 (m :> _) = m

n2 :: Monitor ('M (t0 ': t ': ts)) -> Monitor t
n2 (_ :> m :> _) = m
n2 (_ :> Multi m) = m

n3 :: Monitor ('M (t0 ': t1 ': t ': ts)) -> Monitor t
n3 (_ :> _ :> m :> _) = m
n3 (_ :> _ :> Multi m) = m

notify :: Monitor x -> Double -> Monitor x
notify (Multi m) x = Multi $ notify m x
notify m@(Single _) x = u m x
notify (m :> ms) x = notify m x :> notify ms x

data Monitor :: Tree * -> * where
  Single :: (Storable t) => S t -> Monitor ('Si t)
  Multi :: Monitor t -> Monitor ('M '[t])
  (:>) :: Monitor t
       -> Monitor ('M ts)
       -> Monitor ('M (t ': ts))

infixr 5 :>

-- Instances

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

type family (++) (ls :: [*]) (rs :: [*]) :: [*] where
  (++) ls '[] = ls
  (++) '[] rs = rs
  (++) (l ': ls) rs = l ': (ls ++ rs)

type family (&^) (sub :: *) (over :: * -> *) :: * where
  (&^) sub over = over sub

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

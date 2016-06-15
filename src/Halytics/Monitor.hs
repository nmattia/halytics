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

import Data.List    (intercalate, sort)
import Data.Proxy
import GHC.TypeLits
import Safe

data OneOrList a = Si a | M [a]

class Generate t where
  generate :: Monitors t

instance {-# OVERLAPPING #-} (Storable t) => Generate '[t] where
  generate = MNow monitor

instance (Storable t, Generate ts) => Generate (t ': ts) where
  generate = monitor :< generate

class Generate' t where
  g' :: Monitor' t

instance (Storable' t) => Generate' ('Si t) where
  g' = Single (g proxy)
          where
            proxy :: Proxy t
            proxy = Proxy


instance (Storable' t) => Generate' ('M '[t]) where
  g' = Multi $ Single (g proxy)
    where
      proxy :: Proxy t
      proxy = Proxy

instance (Storable' t, Generate' ('M ts)) => Generate' ('M (t ': ts)) where
  g' = Single (g proxy) :> g'
    where
      proxy :: Proxy t
      proxy = Proxy

class Resultable t r where
  result :: Monitor t -> r

class Storable t where
  type S t
  update :: Monitor t -> Double -> Monitor t
  foobar :: Proxy t -> S t
  monitor :: Monitor t
  monitor = Mon (foobar (Proxy :: Proxy t))

class Resultable' t r where
  r :: Monitor' t -> r

class Storable' t where
  type S' t
  u :: Monitor' ('Si t) -> Double -> Monitor' ('Si t)
  u' :: Proxy t -> S' t -> Double -> S' t
  g :: Proxy t -> S' t
  u (Single s) x = Single $ u' p s x
    where
      p :: Proxy t
      p = Proxy


notii :: Monitor' x -> Double -> Monitor' x
notii (Multi m) x = Multi $ u m x
notii m@(Single _) x = u m x
notii (m :> ms) x = u m x :> notii ms x

data Monitor' :: OneOrList * -> * where
  Single :: (Storable' t) => S' t -> Monitor' ('Si t)
  Multi :: (Storable' t) => Monitor' ('Si t) -> Monitor' ('M '[t])
  (:>) :: (Storable' t)
       => Monitor' ('Si t)
       -> Monitor' ('M ts)
       -> Monitor' ('M (t ': ts))

data Monitor :: * -> * where
  Mon :: (Storable t) => S t -> Monitor t

data Monitors :: [*] -> * where
  MNow :: (Storable t) => Monitor t -> Monitors '[t]
  (:<) :: (Storable t) => Monitor t -> Monitors ts -> Monitors (t ': ts)

infixr 5 :<

-- stop

values :: Monitor t -> S t
values (Mon s) = s

notify :: Monitors ts -> Double -> Monitors ts
notify (MNow m) x = MNow m'
  where m' = update m x
notify (m :< ms) x = m' :< notify ms x
  where m' = update m x

popResult :: (Resultable t r) => Monitors (t ': ts) -> (r, Monitors ts)
popResult (MNow m) = (result m, undefined)
popResult (m :< ms) = (result m, ms)

pop :: Monitors (t ': ts) -> (Monitor t, Maybe (Monitors ts))
pop (MNow m) = (m, Nothing)
pop (m :< ms) = (m, Just ms)

-- Instances

data Max

instance Storable Max where
  type S Max = [Double]
  update (Mon xs) x = Mon $ x:xs
  foobar _ = []

instance Resultable Max (Maybe Double) where
  result m = maximumMay (values m)

instance Resultable Max String where
  result m = maybe naught (\x -> "Max: " ++ show x) res
    where
      naught = "No maximum found"
      res = result m :: Maybe Double

instance Storable' Max where
  type S' Max = [Double]
  g _ = []
  u' _ xs x = x:xs

data Percentile :: Nat -> *

instance Storable (Percentile n) where
  type S (Percentile n) = [Double]
  update (Mon xs) x = Mon $ x:xs
  foobar _ = []

instance (KnownNat n) => Resultable (Percentile n) (Maybe Double) where
  result m = xs `atMay` index
    where
      index = l * n `div` 100
      n = fromInteger $ natVal proxy :: Int
      l = length xs
      proxy = Proxy :: Proxy n
      xs = sort $ values m

instance (KnownNat n) => Resultable (Percentile n) String where
  result m = maybe naught f res
    where
      res = result m :: (Maybe Double)
      naught = "No " ++ show n ++ "th percentile available"
      f p = show n ++ "th percentile: " ++ show p
      n = fromInteger $ natVal proxy :: Int
      proxy = Proxy :: Proxy n

data Last :: Nat -> *

instance (KnownNat n) => Storable (Last n) where
  type S (Last n) = [Double]
  foobar _ = []
  update (Mon xs) x = Mon . take n $ (x:xs)
    where
      n = fromInteger $ natVal proxy :: Int
      proxy = Proxy :: Proxy n

instance Resultable (Last n) [Double] where
  result = values

instance Resultable (Last n) String where
  result m = "Last entries: " ++ entries showed ++ "."
    where
      entries [] = "(none)"
      entries xs = "... " ++ intercalate ", " xs
      showed = show <$> (result m :: [Double])

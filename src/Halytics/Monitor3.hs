{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE EmptyDataDecls         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

module Halytics.Monitor3 where

import Safe
import GHC.TypeLits
import Data.Proxy

class Resultable t r where
  result :: Monitor t -> r

type family S a :: *

type family (++) (ls :: [*]) (rs :: [*]) :: [*] where
  (++) ls '[] = ls
  (++) '[] rs = rs
  (++) (l ': ls) rs = l ': (ls ++ rs)

class Storable t where
  update :: Monitor t -> Double -> Monitor t
  monitor :: Monitor t

data Monitor :: * -> * where
  Mon :: (Storable t) => S t -> Monitor t

data Monitors :: [*] -> * where
  MNow :: (Storable t) => Monitor t -> Monitors '[t]
  {-(:++) :: Monitors tl -> Monitors tr -> Monitors (tl ++ tr)-}
  (:<) :: (Storable t) => Monitor t -> Monitors ts -> Monitors (t ': ts)

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

{-runAll :: Monitors ts -> IO ()-}
{-popResult :: (Resultable)-}
{-execute-}

-- Generation

class Generate t where
  generate :: Monitors t

instance {-# OVERLAPPING #-} (Storable t) => Generate '[t] where
  generate = MNow monitor

instance (Storable t, Generate ts) => Generate (t ': ts) where
  generate = monitor :< generate

-- Instances

data Max

type instance S Max = [Double]

instance Storable Max where
  update (Mon xs) x = Mon $ x:xs
  monitor = Mon []

instance Resultable Max (Maybe Double) where
  result m = maximumMay (values m)

instance Resultable Max String where
  result m = maybe naught (\x -> "Max: " ++ show x) res
    where
      naught = "No maximum found"
      res = result m :: Maybe Double


data Percentile :: Nat -> *

type instance S (Percentile n) = [Double]

instance Storable (Percentile n) where
  update (Mon xs) x = Mon $ x:xs
  monitor = Mon []

instance (KnownNat n) => Resultable (Percentile n) (Maybe Double) where
  result m = xs `atMay` index
    where
      index = l * n `div` 100
      n = fromInteger $ natVal proxy :: Int
      l = length xs
      proxy = Proxy :: Proxy n
      xs = values m

instance (KnownNat n) => Resultable (Percentile n) String where
  result m = maybe naught f res
    where
      res = result m :: (Maybe Double)
      naught = "No " ++ show n ++ "th percentile available"
      f p = show n ++ "th percentile: " ++ show p
      n = fromInteger $ natVal proxy :: Int
      proxy = Proxy :: Proxy n

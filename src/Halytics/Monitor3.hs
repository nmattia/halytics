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

module Halytics.Monitor3 where

import Data.List    (intercalate, sort)
import Data.Proxy
import GHC.TypeLits
import Safe

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
  sorry :: (a ~ S t) => a
  monitor = m
    where m :: Monitor t
          m = Mon s
          s :: a
          s = sorry

data Monitor :: * -> * where
  Mon :: (Storable t) => S t -> Monitor t

data Monitors :: [*] -> * where
  MNow :: (Storable t) => Monitor t -> Monitors '[t]
  {-(:++) :: Monitors tl -> Monitors tr -> Monitors (tl ++ tr)-}
  (:<) :: (Storable t) => Monitor t -> Monitors ts -> Monitors (t ': ts)



{-instance (Stringies ts) => Stringies (t ': ts)-}
{-class (Resultable t String, Stringies ts) => Stringies (t ': ts)-}

-- tests

silly :: Monitors '[Max, Percentile 5]
silly = generate

{-something :: (Resultable t String) => Monitors (t ': ts) -> IO ()-}
{-something (m :< ms) = putStrLn (result m) >> something ms-}
{-something (MNow m) = putStrLn $ result m-}

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

type instance S (Last n) = [Double]

instance (KnownNat n) => Storable (Last n) where
  monitor = Mon []
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

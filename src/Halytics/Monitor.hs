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

data OneOrList a = Si a | M [a] -- TODO: should this be an actual tree? S a | M [OoL a]

class Generate t where
  g' :: Monitor t

instance (Storable t) => Generate ('Si t) where
  g' = Single (g (Proxy :: Proxy t))

instance {-# OVERLAPPING #-} (Storable t) => Generate ('M '[t]) where
  g' = Multi $ Single (g (Proxy :: Proxy t))

instance (Storable t, Generate ('M ts)) => Generate ('M (t ': ts)) where
  g' = Single (g (Proxy :: Proxy t)) :> g'

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

n1 :: Monitor ('M (t ': ts)) -> Monitor ('Si t)
n1 (Multi m) = m
n1 (m :> _) = m

n2 :: Monitor ('M (t0 ': t ': ts)) -> Monitor ('Si t)
n2 (_ :> m :> _) = m
n2 (_ :> Multi m) = m

n3 :: Monitor ('M (t0 ': t1 ': t ': ts)) -> Monitor ('Si t)
n3 (_ :> _ :> m :> _) = m
n3 (_ :> _ :> Multi m) = m

notify :: Monitor x -> Double -> Monitor x
notify (Multi m) x = Multi $ u m x
notify m@(Single _) x = u m x
notify (m :> ms) x = u m x :> notify ms x

data Monitor :: OneOrList * -> * where
  Single :: (Storable t) => S t -> Monitor ('Si t)
  Multi :: (Storable t) => Monitor ('Si t) -> Monitor ('M '[t])
  (:>) :: (Storable t)
       => Monitor ('Si t)
       -> Monitor ('M ts)
       -> Monitor ('M (t ': ts))

infixr 5 :>

pop' :: Monitor ('M (t ': ts)) -> (Monitor ('Si t), Maybe (Monitor ('M ts)))
pop' (Multi m) = (m, Nothing)
pop' (m :> ms) = (m, Just ms)

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

data Last :: Nat -> *

instance (KnownNat n) => Storable (Last n) where
  type S (Last n) = [Double]
  g _ = []
  u' _ xs x = take n (x:xs)
    where
      n = fromInteger $ natVal (Proxy :: Proxy n) :: Int

instance Resultable (Last n) [Double] where
  r _ xs = xs

instance Resultable (Last n) String where
  r _ xs = "Last entries: " ++ entries showed ++ "."
    where
      entries [] = "(none)"
      entries ss = "... " ++ intercalate ", " ss
      showed = show <$> (xs :: [Double])

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

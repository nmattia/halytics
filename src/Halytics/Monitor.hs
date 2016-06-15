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
  g' = Single (g proxy)
          where
            proxy :: Proxy t
            proxy = Proxy

instance {-# OVERLAPPING #-} (Storable t) => Generate ('M '[t]) where
  g' = Multi $ Single (g proxy)
    where
      proxy :: Proxy t
      proxy = Proxy

instance (Storable t, Generate ('M ts)) => Generate ('M (t ': ts)) where
  g' = Single (g proxy) :> g'
    where
      proxy :: Proxy t
      proxy = Proxy

class Resultable t r where
  r :: Proxy t -> S t -> r

class Storable t where
  type S t
  u :: Monitor ('Si t) -> Double -> Monitor ('Si t)
  u' :: Proxy t -> S t -> Double -> S t
  g :: Proxy t -> S t
  u (Single s) x = Single $ u' p s x
    where
      p :: Proxy t
      p = Proxy

result' :: (Resultable t r) => Proxy t -> Monitor ('Si t) -> r
result' p (Single s) = r p s

notii :: Monitor x -> Double -> Monitor x
notii (Multi m) x = Multi $ u m x
notii m@(Single _) x = u m x
notii (m :> ms) x = u m x :> notii ms x

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
      res = r p xs :: Maybe Double
      p :: Proxy Max
      p = Proxy

data Percentile :: Nat -> *

instance Storable (Percentile n) where
  type S (Percentile n) = [Double]
  u' _ xs x = x:xs
  g _ = []

instance (KnownNat n) => Resultable (Percentile n) (Maybe Double) where
  r _ xs = xs' `atMay` index
    where
      index = l * n `div` 100
      n = fromInteger $ natVal proxy :: Int
      l = length xs
      proxy = Proxy :: Proxy n
      xs' = sort xs

instance (KnownNat n) => Resultable (Percentile n) String where
  r _ xs = maybe naught f res
    where
      res = r p xs :: (Maybe Double)
      p :: Proxy (Percentile n)
      p = Proxy
      naught = "No " ++ show n ++ "th percentile available"
      f p = show n ++ "th percentile: " ++ show p
      n = fromInteger $ natVal proxy :: Int
      proxy = Proxy :: Proxy n

{-data Last :: Nat -> *-}

{-instance (KnownNat n) => Storable (Last n) where-}
  {-type S (Last n) = [Double]-}
  {-foobar _ = []-}
  {-update (Mon xs) x = Mon . take n $ (x:xs)-}
    {-where-}
      {-n = fromInteger $ natVal proxy :: Int-}
      {-proxy = Proxy :: Proxy n-}

{-instance Resultable (Last n) [Double] where-}
  {-result = values-}

{-instance Resultable (Last n) String where-}
  {-result m = "Last entries: " ++ entries showed ++ "."-}
    {-where-}
      {-entries [] = "(none)"-}
      {-entries xs = "... " ++ intercalate ", " xs-}
      {-showed = show <$> (result m :: [Double])-}

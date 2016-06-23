{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Halytics.Metric where

import Data.List
import Data.List.Split           (chunksOf)
import Data.Proxy
import GHC.TypeLits
import Halytics.Monitor.Internal

-- |
-- >>> :set -XDataKinds
-- >>> let monitor = generate :: Monitor ('L All)
-- >>> result (collectManyFor monitor [1.0,2.0,3.0]) :: [Double]
-- [1.0,2.0,3.0]
data All :: *

instance Collect All where
  type S All = [Double]
  collect _ = flip (:)

instance Default All where
  initial _ = []

instance Resultable All [Double] where
  r _ = reverse

-- |
-- >>> :set -XDataKinds
-- >>> let monitor = generate :: Monitor ('L Max)
-- >>> result (collectManyFor monitor [1.0,2.0,3.0]) :: Maybe Double
-- Just 3.0
--
-- >>> :set -XDataKinds
-- >>> let monitor = generate :: Monitor ('L Max)
-- >>> result (collectManyFor monitor []) :: Maybe Double
-- Nothing
data Max

instance Collect Max where
  type S Max = Maybe Double
  collect _ (Just !x) x' = Just $ max x x'
  collect _ Nothing x' = Just x'

instance Default Max where
  initial _ = Nothing

instance Resultable Max (Maybe Double) where
  r _ x = x

instance Resultable Max String where
  r _  xs = maybe naught (\x -> "Max: " ++ show x) res
    where
      naught = "No maximum found"
      res = r (Proxy :: Proxy Max) xs :: Maybe Double

-- |
-- >>> :set -XDataKinds
-- >>> let monitor = generate :: Monitor ('L Min)
-- >>> result (collectManyFor monitor [1.0,2.0,3.0]) :: Maybe Double
-- Just 1.0
--
-- >>> :set -XDataKinds
-- >>> let monitor = generate :: Monitor ('L Min)
-- >>> result (collectManyFor monitor []) :: Maybe Double
-- Nothing
data Min

instance Collect Min where
  type S Min = Maybe Double
  collect _ (Just !x) x' = Just $ min x x'
  collect _ Nothing x' = Just x'

instance Default Min where
  initial _ = Nothing

instance Resultable Min (Maybe Double) where
  r _ x = x

instance Resultable Min String where
  r _  xs = maybe naught (\x -> "Min: " ++ show x) res
    where
      naught = "No minimum found"
      res = r (Proxy :: Proxy Min) xs :: Maybe Double

-- Combinators

-- |
-- >>> :set -XDataKinds
-- >>> :set -XTypeOperators
-- >>> let monitor = generate :: Monitor ('L (All |^ Every 2))
-- >>> result (collectManyFor monitor [1.0,2.0,3.0,2.0,1.0]) :: [Double]
-- [1.0,3.0,1.0]
data Every :: Nat -> * -> *

-- TODO: Use pattern guards instead
instance (Collect s, KnownNat n) => Collect (Every n s) where
  type S (Every n s) = (Integer, S s)
  collect _ (1, s) x = (natVal (Proxy :: Proxy n), collect (Proxy :: Proxy s) s x)
  collect _ (n, s) _ = (n-1, s)

instance (KnownNat n, Default s) => Default (Every n s) where
  initial _ = (1, initial (Proxy :: Proxy s))

instance (Resultable t r) => Resultable (Every n t) r where
  r _ (_, s) = r (Proxy :: Proxy t) s

data Last :: Nat -> * -> *

instance (KnownNat n) => Collect (Last n s) where
  type S (Last n s) = [Double]
  collect _ xs x = take n (x:xs)
    where
      n = fromInteger $ natVal (Proxy :: Proxy n) :: Int

instance Default (Last n s) where
  initial _ = []

instance (Default t, Collect t, Resultable t r) => Resultable (Last n t) r where
  r _ xs = r (Proxy :: Proxy t) s
    where
      s = foldl' (collect (Proxy :: Proxy t)) (initial (Proxy :: Proxy t)) xs

data PeriodOf :: Nat -> * -> *

instance Collect (PeriodOf n s) where
  type S (PeriodOf n s) = [Double]
  collect _ = flip (:)

instance Default (PeriodOf n s) where
  initial _ = []

instance (KnownNat n, Resultable t r, Collect t, Default t) => Resultable (PeriodOf n t) [r] where
  r _ xs = r (Proxy :: Proxy t) <$> ss
    where
      ss = foldl' (collect (Proxy :: Proxy t)) (initial (Proxy :: Proxy t)) <$> xss
      xss = chunksOf n (reverse xs)
      n = fromInteger $ natVal (Proxy :: Proxy n) :: Int

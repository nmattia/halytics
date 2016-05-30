{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ScopedTypeVariables     #-}

module Halytics.Monitor where

import GHC.TypeLits
import Data.Proxy

class Resultable a where
  toValue :: [Double] -> Result a

class Generate a where
  generate :: Monitor a

instance (Resultable a) => Generate '[a] where
  generate = MNow []

instance {-# OVERLAPPABLE #-} (Resultable a, Generate as) => Generate (a ': as) where
  generate = MNext generate

data Monitor :: [*] -> * where
  MNow :: (Resultable a) => [Double] -> Monitor '[a]
  MNext :: (Resultable a) => Monitor as -> Monitor (a ': as)

data Result a = Result Double deriving (Show)

data Results :: [*] -> * where
  RNow :: (Resultable a) => Result a -> Results '[a]
  RNext :: (Resultable a) => Result a -> Results as -> Results (a ': as)

instance Show (Results a) where
  show (RNext r rs) = show r ++ show rs
  show (RNow r) = show r

notify :: Monitor a -> Double -> Monitor a
notify (MNow xs) x = MNow (x:xs)
notify (MNext m) x = MNext (notify m x)

toValues :: Monitor as -> Results as
toValues (MNow xs) = RNow $ toValue xs
toValues (MNext m) = RNext (toValue (samples m)) (toValues m)

samples :: Monitor l -> [Double]
samples (MNow xs) = xs
samples (MNext m) = samples m

-- Instances

data Max

instance Resultable Max where
  toValue = Result . maximum

instance Resultable Min where
  toValue = Result . minimum

data Min

data Median

data Percentile :: Nat -> *

-- TODO: this is dummy
instance (KnownNat n) => Resultable (Percentile n) where
  toValue _ = Result . fromIntegral $ natVal proxy
    where proxy = Proxy :: Proxy n

{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}

module Halytics.Monitor where

import GHC.TypeLits

data Result a = Result Double

class Resultable a where
  toValue :: [Double] -> Result a

data Monitor :: [*] -> * where
  MNow :: (Resultable a) => [Double] -> Monitor '[a]
  MNext :: (Resultable a) => Monitor as -> Monitor (a ': as)

data Results :: [*] -> * where
  RNow :: (Resultable a) => Results '[a]
  RNext :: (Resultable a) => Result a -> Results as -> Results (a ': as)

notify :: Monitor a -> Double -> Monitor a
notify (MNow xs) x = MNow (x:xs)
notify (MNext m) x = MNext (notify m x)

toValues :: Monitor as -> Results as
toValues (MNow _) = RNow
toValues (MNext m) = RNext (toValue (samples m)) (toValues m)

samples :: Monitor l -> [Double]
samples (MNow xs) = xs
samples (MNext m) = samples m

-- Instances

data Max

instance Resultable Max where
  toValue = Result . maximum

data Min

data Median

data Percentile :: Nat -> *

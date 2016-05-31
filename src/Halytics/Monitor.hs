{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Halytics.Monitor where

import Data.Proxy
import GHC.TypeLits

class Resultable a a' where
  toValue :: [Double] -> Result a a'

class Generate a where
  generate :: Monitor a

instance (Resultable a r) => Generate '[(a, r)] where
  generate = MNow []

instance {-# OVERLAPPABLE #-} (Resultable a r, Generate as) => Generate ((a, r) ': as) where
  generate = MNext generate

data Monitor :: [*] -> * where
  MNow :: (Resultable a a') => [Double] -> Monitor '[(a, a')]
  MNext :: (Resultable a a') => Monitor as -> Monitor ((a, a') ': as)

data Result a r = Result r deriving (Show)

data Results :: [*] -> * where
  RNow :: (Resultable a r) => Result a r -> Results '[(a, r)]
  RNext :: (Resultable a r) => Result a r -> Results as -> Results ((a, r) ': as)

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

instance Resultable Max String where
  toValue xs = Result $ show res
    where res = toValue xs :: Result Max Double

instance Resultable Max Double where
  toValue = Result . maximum

data Min

instance Resultable Min Double where
  toValue = Result . minimum


data Median

data Percentile :: Nat -> *

-- TODO: this is dummy
instance (KnownNat n, Num a) => Resultable (Percentile n) a where
  toValue _ = Result . fromIntegral $ natVal proxy
    where proxy = Proxy :: Proxy n

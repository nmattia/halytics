{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Halytics.Monitor where

import Data.Proxy
import GHC.TypeLits
import Safe

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
    where res = toValue xs :: Result Max (Maybe Double)

instance Resultable Max (Maybe Double) where
  toValue = Result . maximumMay

data Min

instance Resultable Min (Maybe Double) where
  toValue = Result . minimumMay

type Median = Percentile 50

data Percentile :: Nat -> *

instance (KnownNat n) => Resultable (Percentile n) (Maybe Double) where
  toValue xs = Result $ atMay xs index
    where proxy = Proxy :: Proxy n
          index = l * n `div` 100
          n = fromInteger $ natVal proxy :: Int
          l = length xs

instance (KnownNat n) => Resultable (Percentile n) String where
  toValue xs = Result $ show res
    where res = toValue xs :: Result (Percentile n) (Maybe Double)

instance Show (Results '[]) where
  show _ = ""

instance (Show r, Show (Results as)) => Show (Results ((a, r) ': as)) where
  show (RNow (Result r)) = show r
  show (RNext (Result r) rs) = show r ++ "\n" ++ show rs

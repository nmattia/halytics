{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Halytics.Monitor2 where

import Data.Proxy
import GHC.TypeLits
import Safe

{-class Updatable a b where-}
  {-update :: Double -> b -> b-}
  {-init :: b-}

{-instance Updatable Max (Maybe Double) where-}
  {-update x (Just y) = Just $ max x y-}
  {-update x Nothing = Just x-}
  {-init = Nothing-}

(%>) :: (Resultable a b, Monad m) => m (Monitor (a ': as)) -> (b -> m ()) -> m (Monitor as)
(%>) iom f = iom >>= (\m@(MNext m') -> f (toValue m) >> return m')

infixl 5 %>

plug :: Monitor as -> IO (Monitor as)
plug = return

test :: IO (Monitor '[])
test = plug m %> f1 %> f2 %> f3
  where
    m = generate :: Monitor '[Max, Max, Max]
    f1 = const $ return () :: Maybe Double -> IO ()
    f2 = const $ return () :: Maybe Double -> IO ()
    f3 = const $ return () :: Maybe Double -> IO ()

class Resultable a a' where
  toValue :: Monitor (a ': as) -> a'

class Generate a where
  generate :: Monitor a

instance Generate '[a] where
  generate = MNow []

instance {-# OVERLAPPABLE #-} (Generate as) => Generate (a ': as) where
  generate = MNext generate

data Monitor :: [*] -> * where
  MNow :: [Double] -> Monitor '[a]
  MNext :: Monitor as -> Monitor (a ': as)

data Result a = Result deriving (Show)

notify :: Monitor a -> Double -> Monitor a
notify (MNow xs) x = MNow (x:xs)
notify (MNext m) x = MNext (notify m x)

samples :: Monitor l -> [Double]
samples (MNow xs) = xs
samples (MNext m) = samples m

-- Instances

data Max

instance Resultable Max Double where
  toValue m = maximum (samples m)

instance Resultable Max (Maybe Double) where
  toValue m = maximumMay (samples m)

instance Resultable Max String where
  toValue m = maybe naught f res
    where
      res = toValue m :: (Maybe Double)
      naught = "No maximum available"
      f mx = "Maximum: " ++ show mx

data Min

instance Resultable Min Double where
  toValue m = minimum (samples m)

instance Resultable Min (Maybe Double) where
  toValue m = minimumMay (samples m)

instance Resultable Min String where
  toValue m = maybe naught f res
    where
      res = toValue m :: (Maybe Double)
      naught = "No minimum available"
      f mx = "Minimum: " ++ show mx

type Median = Percentile 50

data Percentile :: Nat -> *

instance (KnownNat n) => Resultable (Percentile n) (Maybe Double) where
  toValue m = xs `atMay` index
    where
      index = l * n `div` 100
      n = fromInteger $ natVal proxy :: Int
      l = length xs
      proxy = Proxy :: Proxy n
      xs = samples m

instance (KnownNat n) => Resultable (Percentile n) String where
  toValue m = maybe naught f res
    where
      res = toValue m :: (Maybe Double)
      naught = "No " ++ show n ++ "th percentile available"
      f p = show n ++ "th percentile: " ++ show p
      n = fromInteger $ natVal proxy :: Int
      proxy = Proxy :: Proxy n

{-# LANGUAGE RankNTypes            #-}

module Halytics.Monitor.Internal where

import Data.Proxy

data Tree a = L a | N [Tree a]

type family (|^) (sub :: *) (over :: * -> *) :: * where
  (|^) sub over = over sub

class Init t where
  g'' :: t -> S t

class Generate t where
  g' :: Monitor t

class Default t where
  g :: Proxy t -> S t

instance (Default t, Storable t) => Generate ('L t) where
  g' = Single (g (Proxy :: Proxy t))

instance {-# OVERLAPPING #-} (Generate t) => Generate ('N '[t]) where
  g' = Multi g'

instance (Generate t, Generate ('N ts)) => Generate ('N (t ': ts)) where
  g' = g' :> g'

class Resultable t r where
  r :: Proxy t -> S t -> r

class Storable t where
  type S t
  u :: Monitor ('L t) -> Double -> Monitor ('L t)
  u' :: Proxy t -> S t -> Double -> S t

  u (Single s) x = Single $ u' (Proxy :: Proxy t) s x
  u' _ s x = case u (Single s :: Monitor ('L t)) x of Single s' -> s'

monitorWith :: (Init t, Storable t) => t -> Monitor ('L t)
monitorWith = Single . g''

result' :: (Resultable t r) => Proxy t -> Monitor ('L t) -> r
result' p (Single s) = r p s

result :: (Resultable t r) => Monitor ('L t) -> r
result = result' (Proxy :: Proxy t)

notify :: Monitor x -> Double -> Monitor x
notify (Multi m) x = Multi $ notify m x
notify m@(Single _) x = u m x
notify (m :> ms) x = notify m x :> notify ms x

-- type family (@@) (ls :: [Tree *]) (rs :: [Tree *]) :: [Tree *]

data Monitor :: Tree * -> * where
  Single :: (Storable t) => S t -> Monitor ('L t)
  Multi :: Monitor t -> Monitor ('N '[t])
  (:>) :: Monitor t
       -> Monitor ('N ts)
       -> Monitor ('N (t ': ts))
--  (:++) :: Monitor ('N ts)
--        -> Monitor ('N ts')
--        -> Monitor ('N ((@@) ts ts'))

infixr 5 :>

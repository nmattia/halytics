{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Halytics.Monitor.Internal where

import Data.Proxy (Proxy (..))

data Placeholder
instance Collect Placeholder where
  type S Placeholder = ()
  collect _ _ _ = ()

instance Default Placeholder where
  initial _ = ()

data Tree a = L a | N [Tree a]

type family (|^) (sub :: *) (over :: * -> *) :: * where
  (|^) sub over = over sub

class Initialize t where
  initialize :: t -> S t

class Generate t where
  generate :: Monitor t

class Default t where
  initial :: Proxy t -> S t

instance (Default t, Collect t) => Generate ('L t) where
  generate = Single (initial (Proxy :: Proxy t))

instance {-# OVERLAPPING #-} (Generate t) => Generate ('N '[t]) where
  generate = Multi generate

instance (Generate t, Generate ('N ts)) => Generate ('N (t ': ts)) where
  generate = generate :> generate

class Resultable t r where
  r :: Proxy t -> S t -> r

class Collect t where
  type S t
  collectFor :: Monitor ('L t) -> Double -> Monitor ('L t)
  collect :: Proxy t -> S t -> Double -> S t

  collectFor (Single s) x = Single $ collect (Proxy :: Proxy t) s x
  collect _ s x = case collectFor (Single s :: Monitor ('L t)) x of Single s' -> s'



-- |
-- Return a result of a particular type.
result :: (Resultable t r) => Monitor ('L t) -> r
result = resultWithProxy (Proxy :: Proxy t)

-- |
-- This shouldn't be used, but somehow GHC refuses to compile if this function
-- doesn't exist:
--
-- @result (Single s) = r (Proxy :: Proxy t) s@
--
resultWithProxy :: (Resultable t r) => Proxy t -> Monitor ('L t) -> r
resultWithProxy p (Single s) = r p s

fromPlaceholder :: (Initialize t, Collect t)
                => Monitor ('L Placeholder)
                -> t
                -> Monitor ('L t)
fromPlaceholder _ = monitorWith

monitorWith :: (Initialize t, Collect t) => t -> Monitor ('L t)
monitorWith = Single . initialize

-- |
-- Notify a tree of monitors.
notify :: Monitor x -> Double -> Monitor x
notify (Multi m) x = Multi $ notify m x
notify m@(Single _) x = collectFor m x
notify (m :> ms) x = notify m x :> notify ms x

-- type family (@@) (ls :: [Tree *]) (rs :: [Tree *]) :: [Tree *]

data Monitor :: Tree * -> * where
  Single :: (Collect t) => S t -> Monitor ('L t)
  Multi :: Monitor t -> Monitor ('N '[t])
  (:>) :: Monitor t
       -> Monitor ('N ts)
       -> Monitor ('N (t ': ts))
--  (:++) :: Monitor ('N ts)
--        -> Monitor ('N ts')
--        -> Monitor ('N ((@@) ts ts'))

infixr 5 :>

{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE EmptyDataDecls         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

module Halytics.Monitor3 where

import Safe

class Resultable t r where
  result :: Monitor t -> r

type family S a :: *

type family (++) (ls :: [*]) (rs :: [*]) :: [*] where
  (++) ls '[] = ls
  (++) '[] rs = rs
  (++) (l ': ls) rs = l ': (ls ++ rs)

class Storable t where
  update :: Double -> Monitor t -> Monitor t
  generate :: Monitor t

data Monitor :: * -> * where
  Mon :: (Storable t) => S t -> Monitor t

data Monitors :: [*] -> * where
  MNow :: (Storable t) => Monitor t -> Monitors '[t]
  (:++) :: Monitors tl -> Monitors tr -> Monitors (tl ++ tr)

values :: Monitor t -> S t
values (Mon s) = s

notify :: Monitors ts -> Double -> Monitors ts
notify (MNow m) x = MNow m'
  where m' = update x m
notify (ls :++ rs) x = notify ls x :++ notify rs x

-- Instances

data Max

type instance S Max = [Double]

instance Storable Max where
  update x (Mon xs) = Mon $ x:xs
  generate = Mon []

instance Resultable Max (Maybe Double) where
  result m = maximumMay (values m)

instance Resultable Max String where
  result m = show res
    where res = result m :: Maybe Double

test :: Monitors '[Max, Max]
test = MNow generate :++ MNow generate

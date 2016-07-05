{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Halytics.Monitor.Tuple where

import Data.Proxy
import Data.List  (foldl')

class Collect t where
  type S t
  collect :: Proxy t -> S t -> Double -> S t
  collectMany :: Proxy t -> S t -> [Double] -> S t
  notify :: Monitor t -> Double -> Monitor t

  notify (Go s) x = Go $ collect (Proxy :: Proxy t) s x
  notifyMany :: Monitor t -> [Double] -> Monitor t
  notifyMany = foldl' notify

  collectMany p = foldl' (collect p)

class Generate t where
  generate :: Monitor t

class Default t where
  initial :: Proxy t -> S t

class Resultable t r where
  r :: Proxy t -> S t -> r

class Initialize t where
  initialize :: t -> S t

data Single a :: * where
  Single :: (Collect t) => S t -> Single t

data Monitor :: * -> * where
  Go :: S t -> Monitor t

--------------------------------------------------------------------------------
-- 'Generate'

instance (Default t, Collect t) => Generate t where
  generate = Go $ initial (Proxy :: Proxy t)

--------------------------------------------------------------------------------
-- 'Default' instances

instance (Default a, Default b) => Default (a, b) where
  initial _ = ( initial (Proxy :: Proxy a)
              , initial (Proxy :: Proxy b) )

instance (Default a, Default b, Default c)
         => Default (a, b, c) where
  initial _ = ( initial (Proxy :: Proxy a)
              , initial (Proxy :: Proxy b)
              , initial (Proxy :: Proxy c) )

instance (Default a, Default b, Default c, Default d)
         => Default (a, b, c, d) where
  initial _ = ( initial (Proxy :: Proxy a)
              , initial (Proxy :: Proxy b)
              , initial (Proxy :: Proxy c)
              , initial (Proxy :: Proxy d) )

instance (Default a, Default b, Default c, Default d, Default e)
         => Default (a, b, c, d, e) where
  initial _ = ( initial (Proxy :: Proxy a)
              , initial (Proxy :: Proxy b)
              , initial (Proxy :: Proxy c)
              , initial (Proxy :: Proxy d)
              , initial (Proxy :: Proxy e) )

instance (Default a, Default b, Default c, Default d, Default e, Default f)
         => Default (a, b, c, d, e, f) where
  initial _ = ( initial (Proxy :: Proxy a)
              , initial (Proxy :: Proxy b)
              , initial (Proxy :: Proxy c)
              , initial (Proxy :: Proxy d)
              , initial (Proxy :: Proxy e)
              , initial (Proxy :: Proxy f) )

--------------------------------------------------------------------------------
-- 'Collect' instances

instance (Collect a, Collect b) => Collect (a, b) where
  type S (a, b) = (S a, S b)
  collect _ (c, c') x =
    (collect (Proxy :: Proxy a) c x, collect (Proxy :: Proxy b) c' x)

instance (Collect a, Collect b, Collect c) => Collect (a, b, c) where
  type S (a, b, c) = (S a, S b, S c)
  collect _ (c, c', c'') x =
    ( collect (Proxy :: Proxy a) c x
    , collect (Proxy :: Proxy b) c' x
    , collect (Proxy :: Proxy c) c'' x )

instance (Collect a, Collect b, Collect c, Collect d)
         => Collect (a, b, c, d) where
  type S (a, b, c, d) = (S a, S b, S c, S d)
  collect _ (c1, c2, c3, c4) x =
    ( collect (Proxy :: Proxy a) c1 x
    , collect (Proxy :: Proxy b) c2 x
    , collect (Proxy :: Proxy c) c3 x
    , collect (Proxy :: Proxy d) c4 x )

instance (Collect a, Collect b, Collect c, Collect d, Collect e)
         => Collect (a, b, c, d, e) where
  type S (a, b, c, d, e) = (S a, S b, S c, S d, S e)
  collect _ (c1, c2, c3, c4, c5) x =
    ( collect (Proxy :: Proxy a) c1 x
    , collect (Proxy :: Proxy b) c2 x
    , collect (Proxy :: Proxy c) c3 x
    , collect (Proxy :: Proxy d) c4 x
    , collect (Proxy :: Proxy e) c5 x )

instance (Collect a, Collect b, Collect c, Collect d, Collect e, Collect f)
         => Collect (a, b, c, d, e, f) where
  type S (a, b, c, d, e, f) = (S a, S b, S c, S d, S e, S f)
  collect _ (c1, c2, c3, c4, c5, c6) x =
    ( collect (Proxy :: Proxy a) c1 x
    , collect (Proxy :: Proxy b) c2 x
    , collect (Proxy :: Proxy c) c3 x
    , collect (Proxy :: Proxy d) c4 x
    , collect (Proxy :: Proxy e) c5 x
    , collect (Proxy :: Proxy f) c6 x )

monitorWith :: (Initialize t, Collect t) => t -> Monitor t
monitorWith = Go . initialize

resultWithProxy :: (Resultable t r) => Proxy t -> Monitor t -> r
resultWithProxy p (Go s) = r p s

result :: (Resultable t r) => Monitor t -> r
result = resultWithProxy (Proxy :: Proxy t)

-------------------------------------------------------------------------------
-- Placeholder

-- | A dummy @Monitor@ for convenience.
-- TODO: Should this go with the lenses?
data Placeholder
instance Collect Placeholder where
  type S Placeholder = ()
  collect _ _ _ = ()

instance Default Placeholder where
  initial _ = ()

-------------------------------------------------------------------------------
-- Useful shizzle

-- | Type family that applies the RHS (@* -> *@) to the LHS (@*@) to produce a
-- type. It is merely syntactic sugar.
-- examples:
-- @
--    Int |^ Monad ≡ Monad Int
--    Max |^ (Last 10) ≡ Last 10 Max
-- @
type family (|^) (sub :: *) (over :: * -> *) :: * where
  (|^) sub over = over sub


fromPlaceholder :: (Initialize t, Collect t)
                => Monitor Placeholder
                -> t -- ^ The initial value
                -> Monitor t
fromPlaceholder _ = monitorWith

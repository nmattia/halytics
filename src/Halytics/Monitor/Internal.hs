{-# LANGUAGE BangPatterns          #-}
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

-- |
-- Module      : Halytics.Monitor.Internal
-- Copyright   : (c) 2016 Nicolas Mattia
-- License     : MIT
-- Maintainer  : Nicolas Mattia <nicolas@nmattia.com>
-- Stability   : experimental
--
-- This module contains the inner workings of a @Monitor@.

module Halytics.Monitor.Internal where

import Data.List  (foldl')
import Data.Proxy (Proxy (..))

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


-- | Simple tree datatype. A tree is either a Leaf (@L@) or a Node (@N@). A
-- leaf holds a value of type @a@. A node holds a forest (@[Tree a]@, a list of
-- trees).
data Tree a = L a | N [Tree a]

-------------------------------------------------------------------------------
-- Monitor


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



-------------------------------------------------------------------------------
-- Typeclasses

-- | The class of types that can be monitored.
class Collect t where

  {-# MINIMAL (collect | collectFor) #-}

  -- | The inner representation of the monitored value.
  type S t


  -- | Collect a value for a specific monitored type, updating the inner
  -- representation.
  collect :: Proxy t -> S t -> Double -> S t

  -- | Collect a value for a specific monitor. Equalivalent to 'collect', but
  -- acting on the leaf of a @Monitor@ @Tree@.
  collectFor :: Monitor ('L t) -> Double -> Monitor ('L t)

  -- | Collect a list of values for a specific monitored type (batch
  -- collecting).
  collectMany :: Proxy t -> S t -> [Double] -> S t

  -- | Collect a list of values for a specific monitor (batch collecting).
  collectManyFor :: Monitor ('L t) -> [Double] -> Monitor ('L t)

  collect _ s x =
    case collectFor (Single s :: Monitor ('L t)) x of Single s' -> s'
  collectFor (Single s) x = Single $ collect (Proxy :: Proxy t) s x
  collectMany p = foldl' f
    where
      f !s !x = collect p s x
  collectManyFor = foldl' f
    where
      f m !x = collectFor m x


-- | The class of types that have a constant value for their initial inner
-- representation.
class Default t where
  initial :: Proxy t -> S t

-- | The class of types for which a @Monitor@ can be generated without an
-- initializing value. This is used internally, and the user shouldn't not have
-- to care about it.
class Generate t where
  generate :: Monitor t

-- Generate a leaf for a monitor with default inner representation.
instance (Default t, Collect t) => Generate ('L t) where
  generate = Single (initial (Proxy :: Proxy t))

-- Generate a node for a single monitor.
instance {-# OVERLAPPING #-} (Generate t) => Generate ('N '[t]) where
  generate = Multi generate

-- Grow the forest of a node.
instance (Generate t, Generate ('N ts)) => Generate ('N (t ': ts)) where
  generate = generate :> generate

-- | The class representing the results that can be read from a @Monitor@.
-- TODO: 'r' is not a good name.
class Resultable t r where
  {-# MINIMAL r #-}

  -- | Computes a result for a given type. Uses the @Monitor@'s inner
  -- representation.
  r :: Proxy t -> S t -> r

-- | A typeclass for types that are initialized from a value.
class Initialize t where
  initialize :: t -> S t


--------------------------------------------------------------------------------
-- Functions

-- | Return a result of a particular type.
result :: (Resultable t r) => Monitor ('L t) -> r
result = resultWithProxy (Proxy :: Proxy t)

-- | This shouldn't be used, but somehow GHC refuses to compile if this
-- function doesn't exist. The following doesn't work:
--
-- @result (Single s) = r (Proxy :: Proxy t) s@
resultWithProxy :: (Resultable t r) => Proxy t -> Monitor ('L t) -> r
resultWithProxy p (Single s) = r p s

-- | Replaces a @Placeholder@ @Monitor@ with an initializable one. This is a
-- convenience function, and is used internally. The library user should not
-- have to care about it.
-- @fromPlaceholder _ = monitorWith@
fromPlaceholder :: (Initialize t, Collect t)
                => Monitor ('L Placeholder)
                -> t -- ^ The initial value
                -> Monitor ('L t)
fromPlaceholder _ = monitorWith

-- | Initializes an initializable @Monitor@ with the provided value. This is a
-- wrapper around 'initialize' and is used to hide the @Monitor@ constructor.
monitorWith :: (Initialize t, Collect t) => t -> Monitor ('L t)
monitorWith = Single . initialize

-- | Notify a tree of monitors. Collects for all the monitors in the tree.
notify :: Monitor x -> Double -> Monitor x
notify (Multi m) x = Multi $ notify m x
notify m@(Single _) x = collectFor m x
notify (m :> ms) x = notify m x :> notify ms x

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

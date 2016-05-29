{-# LANGUAGE GADTs #-}
module Halytics.Internal where

class Analyzable a where

data Collector where
  Latency :: Collector


{-# LANGUAGE ExplicitNamespaces #-}

module Halytics.Monitor
  ( I.Collect (..)
  , I.Default (..)
  , I.Initialize (..)
  , I.Monitor
  , I.Resultable (..)
  , type (I.|^)
  , I.fromPlaceholder
  , I.generate
  , I.monitorWith
  , I.result
  , (L.%<~)) where


import qualified Halytics.Monitor.Tuple as I
import qualified Halytics.Monitor.Lens as L

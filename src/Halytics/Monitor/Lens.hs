{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Halytics.Monitor.Lens where

import Control.Lens
import Halytics.Monitor.Tuple (Collect (..), Initialize, Monitor (..),
                               Placeholder, fromPlaceholder)

import Halytics.Monitor.Lens.TH

infixr 4 %<~

(%<~) :: (Collect t', Initialize t')
      => ASetter s t (Monitor Placeholder) (Monitor t')
      -> t'
      -> s
      -> t
(%<~) = initializeWith

initializeWith :: (Collect t', Initialize t')
               => ASetter s t (Monitor  Placeholder) (Monitor  t')
               -> t'
               -> s
               -> t
initializeWith s val = over s (`fromPlaceholder` val)

-- instance Field1 (Monitor (t1, t2))
                -- (Monitor (t1', t2))
                -- (Monitor t1)
                -- (Monitor t1') where
  -- -- _1 k (Go (a, b)) = k (Go a) <&> (\(Go a') -> Go (a', b))
  -- _1 k (Go (m1, m2)) = fmap (\(Go m1') -> Go (m1', m2)) (k (Go m1))

-- instance Field1 (Monitor (t1, t2, t3))
                -- (Monitor (t1', t2, t3))
                -- (Monitor t1)
                -- (Monitor t1') where
  -- _1 k (Go (a, b, c)) = k (Go a) <&> (\(Go a') -> Go (a', b, c))

-- instance Field1 (Monitor (t1, t2, t3, t4))
                -- (Monitor (t1', t2, t3, t4))
                -- (Monitor t1)
                -- (Monitor t1') where
  -- _1 k (Go (a, b, c, d)) = k (Go a) <&> (\(Go a') -> Go (a', b, c, d))

-- instance Field1 (Monitor (t1, t2, t3, t4, t5))
                -- (Monitor (t1', t2, t3, t4, t5))
                -- (Monitor t1)
                -- (Monitor t1') where
  -- _1 k (Go (a, b, c, d, e)) = k (Go a) <&> (\(Go a') -> Go (a', b, c, d, e))

-- instance Field1 (Monitor (t1, t2, t3, t4, t5, t6))
                -- (Monitor (t1', t2, t3, t4, t5, t6))
                -- (Monitor t1)
                -- (Monitor t1') where
  -- _1 k (Go (a, b, c, d, e, f)) =
  -- k (Go a) <&> (\(Go a') -> Go (a', b, c, d, e, f))

-- instance Field2 (Monitor (t1, t2))
                -- (Monitor (t1, t2'))
                -- (Monitor t2)
                -- (Monitor t2') where
  -- _2 k (Go (a, b)) = k (Go b) <&> (\(Go b') -> Go (a, b'))

-- instance Field2 (Monitor (t1, t2, t3))
                -- (Monitor (t1, t2', t3))
                -- (Monitor t2)
                -- (Monitor t2') where
  -- _2 k (Go (m1, m2, m3)) = k (Go m2) <&> (\(Go m2') -> Go (m1, m2', m3))

$(plop)

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Halytics.Monitor.Lens where

import Control.Lens              (ASetter, lens, over)
import Control.Lens.Tuple        (Field1, Field2, Field3, Field4, Field5,
                                  Field6, _1, _2, _3, _4, _5, _6)
import Halytics.Monitor.Internal (Collect (..), Initialize, Monitor (..),
                                  Placeholder, Tree (..), fromPlaceholder)

infixr 4 %<~

(%<~) :: (Collect t', Initialize t')
      => ASetter s t (Monitor ('L Placeholder)) (Monitor ('L t'))
      -> t'
      -> s
      -> t
(%<~) = initializeWith

initializeWith :: (Collect t', Initialize t')
               => ASetter s t (Monitor ('L Placeholder)) (Monitor ('L t'))
               -> t'
               -> s
               -> t
initializeWith s val = over s (`fromPlaceholder` val)

instance Field1 (Monitor ('N (t ': ts)))
                (Monitor ('N (t' ': ts)))
                (Monitor t)
                (Monitor t') where
  _1 = lens pull1 replace1

pull1 :: Monitor ('N (t ': ts)) -> Monitor t
pull1 (Multi m) = m
pull1 (m :> _) = m

replace1 :: Monitor ('N (t ': ts))
         -> Monitor t'
         -> Monitor ('N (t' ': ts))
replace1 (Multi _) m = Multi m
replace1 (_ :> ms) m = m :> ms

instance Field2 (Monitor ('N (t0 ': t1  ': ts)))
                (Monitor ('N (t0 ': t1' ': ts)))
                (Monitor t1)
                (Monitor t1') where
  _2 = lens pull2 replace2


pull2 :: Monitor ('N (t0 ': t1 ': ts)) -> Monitor t1
pull2 (_ :> ms) = pull1 ms

replace2 :: Monitor ('N (t0 ': t1 ': ts))
         -> Monitor t1'
         -> Monitor ('N (t0 ': t1' ': ts))
replace2 (m0 :> ms) m1 = m0 :> replace1 ms m1


instance Field3 (Monitor ('N (t0 ': t1 ': t2 ': ts)))
                (Monitor ('N (t0 ': t1 ': t2' ': ts)))
                (Monitor t2)
                (Monitor t2') where
  _3 = lens pull3 replace3


pull3 :: Monitor ('N (t0 ': t1 ':t2 ': ts)) -> Monitor t2
pull3 (_ :> ms) = pull2 ms

replace3 :: Monitor ('N (t0 ': t1 ': t2 ': ts))
         -> Monitor t2'
         -> Monitor ('N (t0 ': t1 ': t2' ': ts))
replace3 (m0 :> ms) m1 = m0 :> replace2 ms m1

instance Field4 (Monitor ('N (t0 ': t1 ': t2 ': t3 ': ts)))
                (Monitor ('N (t0 ': t1 ': t2 ': t3' ': ts)))
                (Monitor t3)
                (Monitor t3') where
  _4 = lens pull4 replace4

pull4 :: Monitor ('N (t0 ': t1 ': t2 ': t3 ': ts)) -> Monitor t3
pull4 (_ :> ms) = pull3 ms

replace4 :: Monitor ('N (t0 ': t1 ': t2 ': t3 ': ts))
         -> Monitor t3'
         -> Monitor ('N (t0 ': t1 ': t2 ': t3' ': ts))
replace4 (m0 :> ms) m = m0 :> replace3 ms m

instance Field5 (Monitor ('N (t0 ': t1 ': t2 ': t3 ': t4 ': ts)))
                (Monitor ('N (t0 ': t1 ': t2 ': t3 ': t4' ': ts)))
                (Monitor t4)
                (Monitor t4') where
  _5 = lens pull5 replace5

pull5 :: Monitor ('N (t0 ': t1 ': t2 ': t3 ': t4 ': ts))
      -> Monitor t4
pull5 (_ :> ms) = pull4 ms

replace5 :: Monitor ('N (t0 ': t1 ': t2 ': t3 ': t4 ': ts))
         -> Monitor t4'
         -> Monitor ('N (t0 ': t1 ': t2 ': t3 ': t4' ': ts))
replace5 (m0 :> ms) m = m0 :> replace4 ms m

instance Field6 (Monitor ('N (t0 ': t1 ': t2 ': t3 ': t4 ': t5 ': ts)))
                (Monitor ('N (t0 ': t1 ': t2 ': t3 ': t4 ': t5' ': ts)))
                (Monitor t5)
                (Monitor t5') where
  _6 = lens pull6 replace6

pull6 :: Monitor ('N (t0 ': t1 ': t2 ': t3 ': t4 ': t5 ': ts))
      -> Monitor t5
pull6 (_ :> ms) = pull5 ms

replace6 :: Monitor ('N (t0 ': t1 ': t2 ': t3 ': t4 ': t5 ': ts))
         -> Monitor t5'
         -> Monitor ('N (t0 ': t1 ': t2 ': t3 ': t4 ': t5' ': ts))
replace6 (m0 :> ms) m = m0 :> replace5 ms m

-- TODO: remove boilerplate
{-data Z-}
{-data S a-}

{-type N0 = Z-}
{-type N1 = S N0-}
{-type N2 = S N1-}
{-type N3 = S N2-}
{-type N4 = S N3-}
{-type N5 = S N4-}
{-type N6 = S N5-}
{-type N7 = S N6-}
{-type N8 = S N7-}

{-type family NSize m-}
{-type instance NSize (Monitor ('N '[])) = N0-}
{-type instance NSize (Monitor ('N (t ': ts))) = S (NSize (Monitor ('N ts)))-}

{-type family TypeAt ix (m :: [Tree *]) :: Tree *-}
{-type instance TypeAt N1 (t ': ts) = t-}

{-pull1' :: (t ~ TypeAt N1 ts) => Monitor ('N ts) -> Monitor t-}
{-pull1' (Multi m) = m-}
{-pull1' (m :> _) = m-}

{-pull2' :: (t ~ TypeAt N2 ts) => Monitor ('N ts) -> Monitor t-}
{-pull2' (m :> ms) = undefined-}
{-pullN :: (t ~ TypeAt n ts) => Proxy n -> Monitor ('N ts) -> Monitor t-}
{-pullN (_ :: Proxy N1) _ = undefined-}

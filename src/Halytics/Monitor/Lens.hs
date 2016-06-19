{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Halytics.Monitor.Lens where

import Control.Lens              (lens)
import Control.Lens.Tuple        (Field1, Field2, Field3, Field4, Field5,
                                  Field6, _1, _2, _3, _4, _5, _6)
import Halytics.Monitor.Internal

instance Field1 (Monitor ('N ('L t ': ts)))
                (Monitor ('N ('L t' ': ts)))
                (Monitor ('L t))
                (Monitor ('L t')) where
  _1 = lens pull1 replace1

pull1 :: Monitor ('N ('L t ': ts)) -> Monitor ('L t)
pull1 (Multi m) = m
pull1 (m :> _) = m

replace1 :: Monitor ('N ('L t ': ts))
         -> Monitor ('L t')
         -> Monitor ('N ('L t' ': ts))
replace1 (Multi _) m = Multi m
replace1 (_ :> ms) m = m :> ms

instance Field2 (Monitor ('N ('L t0 ': 'L t1  ': ts)))
                (Monitor ('N ('L t0 ': 'L t1' ': ts)))
                (Monitor ('L t1))
                (Monitor ('L t1')) where
  _2 = lens pull2 replace2


pull2 :: Monitor ('N ('L t0 ': 'L t1 ': ts)) -> Monitor ('L t1)
pull2 (_ :> ms) = pull1 ms

replace2 :: Monitor ('N ('L t0 ': 'L t1 ': ts))
         -> Monitor ('L t1')
         -> Monitor ('N ('L t0 ': 'L t1' ': ts))
replace2 (m0 :> ms) m1 = m0 :> replace1 ms m1


instance Field3 (Monitor ('N ('L t0 ': 'L t1 ': 'L t2 ': ts)))
                (Monitor ('N ('L t0 ': 'L t1 ': 'L t2' ': ts)))
                (Monitor ('L t2))
                (Monitor ('L t2')) where
  _3 = lens pull3 replace3


pull3 :: Monitor ('N ('L t0 ': 'L t1 ': 'L t2 ': ts)) -> Monitor ('L t2)
pull3 (_ :> ms) = pull2 ms

replace3 :: Monitor ('N ('L t0 ': 'L t1 ': 'L t2 ': ts))
         -> Monitor ('L t2')
         -> Monitor ('N ('L t0 ': 'L t1 ': 'L t2' ': ts))
replace3 (m0 :> ms) m1 = m0 :> replace2 ms m1

instance Field4 (Monitor ('N ('L t0 ': 'L t1 ': 'L t2 ': 'L t3 ': ts)))
                (Monitor ('N ('L t0 ': 'L t1 ': 'L t2 ': 'L t3' ': ts)))
                (Monitor ('L t3))
                (Monitor ('L t3')) where
  _4 = lens pull4 replace4

pull4 :: Monitor ('N ('L t0 ': 'L t1 ': 'L t2 ': 'L t3 ': ts)) -> Monitor ('L t3)
pull4 (_ :> ms) = pull3 ms

replace4 :: Monitor ('N ('L t0 ': 'L t1 ': 'L t2 ': 'L t3 ': ts))
         -> Monitor ('L t3')
         -> Monitor ('N ('L t0 ': 'L t1 ': 'L t2 ': 'L t3' ': ts))
replace4 (m0 :> ms) m = m0 :> replace3 ms m

instance Field5 (Monitor ('N ('L t0 ': 'L t1 ': 'L t2 ': 'L t3 ': 'L t4 ': ts)))
                (Monitor ('N ('L t0 ': 'L t1 ': 'L t2 ': 'L t3 ': 'L t4' ': ts)))
                (Monitor ('L t4))
                (Monitor ('L t4')) where
  _5 = lens pull5 replace5

pull5 :: Monitor ('N ('L t0 ': 'L t1 ': 'L t2 ': 'L t3 ': 'L t4 ': ts))
      -> Monitor ('L t4)
pull5 (_ :> ms) = pull4 ms

replace5 :: Monitor ('N ('L t0 ': 'L t1 ': 'L t2 ': 'L t3 ': 'L t4 ': ts))
         -> Monitor ('L t4')
         -> Monitor ('N ('L t0 ': 'L t1 ': 'L t2 ': 'L t3 ': 'L t4' ': ts))
replace5 (m0 :> ms) m = m0 :> replace4 ms m

instance Field6 (Monitor ('N ('L t0 ': 'L t1 ': 'L t2 ': 'L t3 ': 'L t4 ': 'L t5 ': ts)))
                (Monitor ('N ('L t0 ': 'L t1 ': 'L t2 ': 'L t3 ': 'L t4 ': 'L t5' ': ts)))
                (Monitor ('L t5))
                (Monitor ('L t5')) where
  _6 = lens pull6 replace6

pull6 :: Monitor ('N ('L t0 ': 'L t1 ': 'L t2 ': 'L t3 ': 'L t4 ': 'L t5 ': ts))
      -> Monitor ('L t5)
pull6 (_ :> ms) = pull5 ms

replace6 :: Monitor ('N ('L t0 ': 'L t1 ': 'L t2 ': 'L t3 ': 'L t4 ': 'L t5 ': ts))
         -> Monitor ('L t5')
         -> Monitor ('N ('L t0 ': 'L t1 ': 'L t2 ': 'L t3 ': 'L t4 ': 'L t5' ': ts))
replace6 (m0 :> ms) m = m0 :> replace5 ms m

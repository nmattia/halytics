{-# OPTIONS_GHC -fno-warn-unused-imports #-}


module Halytics.Tutorial where

import Control.Lens
import Halytics.Monitor
import Halytics.Metric
import Halytics.Metric.Statistics

{-$

First, some extensions:
>>> :set -XDataKinds

>>> let maxMonitor = notifyMany (generate :: Monitor Max) [1,3,42,-5]
>>> result maxMonitor :: Maybe Double
Just 42.0

>>> let maxMonitor' = notify maxMonitor 42.1
>>> result maxMonitor' :: Maybe Double
Just 42.1

>>> type AliceMetrics = (Min, Max, All)
>>> let alice = generate :: Monitor AliceMetrics
>>> alice^._1&result :: String
"No minimum found"

>>> alice^._3&result :: String
"Collected: (none)"

>>> type BobMetrics = (Median, Percentile 95, Percentile 99)
>>> let bob = generate :: Monitor BobMetrics
>>> bob^._2&result :: Maybe Double
Nothing

>>> type CompanyMetrics = (AliceMetrics, BobMetrics)
>>> let company = generate :: Monitor CompanyMetrics
>>> company^._1._3&result :: String
"Collected: (none)"

-}




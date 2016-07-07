{-# OPTIONS_GHC -fno-warn-unused-imports #-}


module Halytics.Tutorial where

import Control.Lens
import Halytics.Monitor
import Halytics.Metric
import Halytics.Metric.Statistics

{-$

First, some extensions:

>>> :set -XDataKinds

== The basics

The most important type in Halytics is a 'Monitor'. It stores the values you
give it and, when you ask for it, gives you a result. To get a 'Monitor' you
usually call 'generate', which will take care of setting up everything for you.

>>> :t generate
generate :: (Default t, Collect t) => Monitor t

Let's leave the constraint aside for now. What you need to do is simply tell
'generate' what /type/ of 'Monitor' you want and it will build one for you.
Let's start with a simple type, 'Max', that can be found in 'Halytics.Metric'
('Max' works within the constraint @(Default t, Collect t)@):

>>> let firstMonitor = generate :: Monitor Max

There are two functions important to the use of a 'Monitor':

 * 'notify': notify a 'Monitor' that a new value is available for collection.
 * 'result': computes a type of result using the values collected.

Let's have a look at 'notify' first:

>>> :t notify
notify :: Collect t => Monitor t -> Double -> Monitor t


As expected, 'notify' takes a 'Monitor', a new value, and returns a 'Monitor'
of the same type. The internal state of the 'Monitor' was updated to account
for the new value. In the case of @firstMonitor@, @t = Max@. Let's add some
values to our monitor:

>>> let firstMonitor' = (`notify` 3) . (`notify` 1) $ firstMonitor

Here we successively notified the monitor of the values @1@ and @3@. Now, let's
use 'result', mentioned above:

>>> result firstMonitor' :: Maybe Double
Just 3.0

Well, that's good. As expected, the maximum of @1@ and @3@ is... @3@. What
about adding a value?

>>> result $ notify firstMonitor' 42 :: Maybe Double
Just 42.0

Now let's have a quick look at the signature of 'result':

>>> :t result
result :: Resultable t r => Monitor t -> r


'Resultable' is a type class defining a relation between a type of metric (@t@)
and a type of result it can produce (@r@). When we called 'result' above we
used the instance

> instance Resultable Max (Maybe Double) where ...

We could also have used:

>>> result firstMonitor' :: String
"Max: 3.0"

which makes it convenient to print out results. Finally, using our initial
monitor (which doesn't hold any value) we understand why 'result' uses 'Maybe'
'Double' instead of just 'Double' for 'Max':

>>> result firstMonitor :: String
"No maximum found"

The maximum is not defined when there are no values.







== Bundling metrics together


Often you will want to record several metrics over the same set of data. A
'Monitor' doesn't limit you to a single 'Metric'. For instance, Alice wants to
record the minimum, the maximum, and the list of all values that have been
collected.

>>> type AliceMetrics = (Min, Max, All)
>>> let alice = generate :: Monitor AliceMetrics

We know 'Max', and we can infer what 'Min' does. 'All' is different in the
sense that it will keep track of /all/ the values that have been collected so
far. Now we will need to access the metrics. For this, some lenses are
provided:

>>> alice^._1&result :: String
"No minimum found"

>>> alice^._3&result :: String
"Collected: (none)"


Let's see what we're accessing here:

>>> :t alice^._1
alice^._1 :: Monitor Min

>>> :t alice^._2
alice^._2 :: Monitor Max

>>> :t alice^._3
alice^._3 :: Monitor All

Great! Using instances of lens' 'Field1', 'Field2', ... we can jump inside a
'Monitor' @(a, b, ...)@ and use each individual 'Monitor' @a, b, c...@. Still,
you will only need to notify one monitor:

>>> let alice' = (`notify` 9) . (`notify` 3) $ alice
>>> alice'^._1&result :: String
"Min: 3.0"
>>> alice'^._3&result :: String
"Collected: 3.0, 9.0"


Enters Bob, who wants to record the median, @95th@ and @99th@ percentiles:

>>> type BobMetrics = (Median, Percentile 95, Percentile 99)
>>> let bob = generate :: Monitor BobMetrics

'Median' and 'Percentile' are defined in 'Halytics.Metric.Statistics'.
'Percentile' allows you to define the percentile you want to read at compile
time:

>>> :kind Percentile
Percentile :: GHC.TypeLits.Nat -> *

(The median is simply defined as @type Median = Percentile 50@)

Now comes Bob and Alice's boss, who wants to use all those metrics together:

>>> type CompanyMetrics = (AliceMetrics, BobMetrics)
>>> let company = generate :: Monitor CompanyMetrics
>>> let company' = notifyMany company [1, 3.43, -5, 103.0]

And we can still use the individual metrics by combining the lenses:

>>> company'^._1._3&result :: String
"Collected: 1.0, 3.43, -5.0, 103.0"

== Combinators

TODO

== The type classes

TODO

-}




module Server where

import Control.DeepSeq   (force)
import Control.Exception (evaluate)

data Server = Server (Int -> Int)

slowSum :: Int -> Int
slowSum n = sum [1 .. n]

fastSum :: Int -> Int
fastSum n = n * (n + 1) `div` 2

request :: Server -> Int -> IO Int
request (Server f) x = evaluate $ force res
  where res = f x

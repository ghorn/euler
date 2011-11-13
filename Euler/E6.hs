-- E6.hs

{-# OPTIONS_GHC -Wall #-}

module Euler.E6(e6) where

-- difference between sum of squares and square of sum of [1..100]
e6 :: Integer
e6 = (sqrSum 100) - (sumSqr 100)
  where
    sumSqr n = sum $ map (\x -> x*x) [1..n]
    sqrSum n = (\x -> x*x) $ sum [1..n]

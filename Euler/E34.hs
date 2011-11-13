-- E34.hs

{-# OPTIONS_GHC -Wall #-}

module Euler.E34(e34) where

-- (non-working brute force solution) sum of all numbers whose sum of factorial of digits == number
e34 :: Integer
e34 = sum $ filter good [1..10000]
  where
    splitIntegral :: (Integral a, Read a) => a -> [a]
    splitIntegral a = map (\x -> read x) $ map (\x -> [x]) (show a)

    fact :: (Integral a) => a -> a
    fact 0 = 1
    fact n = n *(fact (n-1))

    sumFact n = sum $ map fact (splitIntegral n)
    good :: Integer -> Bool
    good n = (sumFact n) == n

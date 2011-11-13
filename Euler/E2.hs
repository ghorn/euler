-- E2.hs

{-# OPTIONS_GHC -Wall #-}

module Euler.E2(e2) where

e2 :: Integer
e2 = sum $ filter even (fibsUnder 4000000 [])
  where
    fibsUnder :: Integer -> [Integer] -> [Integer]
    fibsUnder n []  = fibsUnder n [1]
    fibsUnder n [x] = fibsUnder n (2:[x])
    fibsUnder n (x:xs)
      | x + (head xs) > n    = (x:xs)
      | otherwise = fibsUnder n ((x + (head xs)):(x:xs))

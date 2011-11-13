-- E1.hs

{-# OPTIONS_GHC -Wall #-}

module Euler.E1(e1) where

e1 :: Integer
e1 = sum $ filter (\x -> divisible x 5 || divisible x 3) [1..999]
  where
    divisible :: (Integral a) => a -> a -> Bool
    divisible a b = if mod a b == 0 then True else False

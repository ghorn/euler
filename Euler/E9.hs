-- E9.hs

{-# OPTIONS_GHC -Wall #-}

module Euler.E9(e9) where

-- only pythagorean triplet with perimiter == 1000
e9 :: Integer
e9 = head [ a*b*c | a <- [1..1000], b <- [1..a], c <- [1000 - a - b], a*a + b*b == c*c]

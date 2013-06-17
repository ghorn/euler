-- E9.hs

{-# OPTIONS_GHC -Wall #-}

module Euler.E009 ( e009 ) where

-- only pythagorean triplet with perimiter == 1000
e009 :: Integer
e009 = head [ a*b*c | a <- [1..1000], b <- [1..a], c <- [1000 - a - b], a*a + b*b == c*c]

-- E4.hs

{-# OPTIONS_GHC -Wall #-}

module Euler.E4(e4) where

e4 :: Integer
e4 = maximum [a*b | a <- [100..999], b <- [100..a], show (a*b) == reverse (show (a*b))]

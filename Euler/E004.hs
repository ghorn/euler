-- E4.hs

{-# OPTIONS_GHC -Wall #-}

module Euler.E004 ( e004 ) where

e004 :: Integer
e004 = maximum [a*b | a <- [100..999], b <- [100..a], show (a*b) == reverse (show (a*b))]

-- E7.hs

{-# OPTIONS_GHC -Wall #-}

module Euler.E007 ( e007 ) where

import Euler.Primes ( infinitePrimes )

-- the 10,001st prime number
e007 :: Integer
e007 = infinitePrimes !! 10000

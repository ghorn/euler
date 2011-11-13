-- E3.hs

{-# OPTIONS_GHC -Wall #-}

module Euler.E3(e3) where

e3 :: Integer
e3 = head $ factorize 600851475143 []
  where
    factorize :: Integer -> [Integer] -> [Integer]
    factorize 1 knownFactors = knownFactors
    factorize n knownFactors = factorize (n `div` firstDivisor) (firstDivisor:knownFactors)
      where
        firstDivisor = head $ filter (\x -> mod n x == 0) [2..n]

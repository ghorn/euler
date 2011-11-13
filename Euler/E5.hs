-- E5.hs

{-# OPTIONS_GHC -Wall #-}

module Euler.E5(e5) where

import Data.List(group)
import qualified Data.Map

-- smallest number that is evenly divisible by each of [1..20]
e5 :: Integer
e5 = foldl mult 1 greatestPrimeFactors
  where
    mult acc (factor, order) = product $ acc:(replicate order factor)

    greatestPrimeFactors = Data.Map.toList $ foldl f Data.Map.empty (concat primeFactorizations)
      where
        f acc x = Data.Map.unionWith max acc (Data.Map.fromList [x])

    primeFactorizations = map ((flip factorize) []) [2..20]
      where
        factorize 1 knownFactors = map (\x -> (head x, length x)) $ group knownFactors
        factorize n knownFactors = factorize (n `div` firstDivisor) (firstDivisor:knownFactors)
          where
            firstDivisor = head $ filter (\x -> mod n x == 0) [2..n]

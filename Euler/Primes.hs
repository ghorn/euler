{-# OPTIONS_GHC -Wall #-}

module Euler.Primes ( infinitePrimes
                    , basicSieve
                    ) where

import qualified Data.Vector.Mutable as MV
import Control.Monad.ST ( runST )

data PrimeStatus = NotPrime | PrimeCandidate

-- | infinite list of prime numbers
--   from "The Genuine Sieve of Eratosthenes" by Melissa E. O'Neill
infinitePrimes :: [Integer]
infinitePrimes = 2 : [ x | x <- [3..], isPrime x]
  where
    isPrime x = all (\p -> x `mod` p > 0 ) (factorsToTry x)
    factorsToTry x = takeWhile (\p -> p*p <= x) infinitePrimes

-- | returns a list of all prime numbers <= a given number
basicSieve :: Int -> [Int]
basicSieve n
  | n < 2 = []
basicSieve n =
  let runSieve acc m k
        | k >= MV.length m = return acc
      runSieve acc m k = do
        val <- MV.read m k
        newAcc <- case val of
          PrimeCandidate -> do
            setMultiples m k
            return (k:acc)
          NotPrime -> return acc
        runSieve newAcc m (k+1)

      setMultiples m k = mapM_ (flip (MV.write m) NotPrime) [k,2*k..MV.length m - 1]

  in runST $ do
    m <- MV.new (n+1)
    MV.set m PrimeCandidate
    MV.write m 0 NotPrime
    MV.write m 1 NotPrime
    primes <- runSieve [] m 2
    return (reverse primes)


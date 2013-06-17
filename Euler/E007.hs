-- E7.hs

{-# OPTIONS_GHC -Wall #-}

module Euler.E007 ( e007, e007' ) where

-- the 10,001st prime number
e007 :: Integer
e007 = fromIntegral $ head $ nPrimes 10001
  where
    nPrimes :: Int -> [Int]
    nPrimes n = primeSieve n [] [2..]
    
    primeSieve :: Int -> [Int] -> [Int] -> [Int]
    primeSieve _ sieved [] = sieved
    primeSieve n sieved (x:xs) 
      | length sieved == n = sieved
      | isPrime x   = primeSieve n (x:sieved) xs
      | otherwise   = primeSieve n sieved xs
      where
        isPrime x' = and $ map (\a -> mod x' a /= 0) sieved

-- better version from from "The Genuine Sieve of Eratosthenes" by Melissa E. O'Neill
e007' :: Integer
e007' = nthPrime 10001
  where
    nthPrime n = primes !! (n - 1)
    primes = ( 2 : [ x | x <- [3..], isPrime x])
    isPrime x = all (\p -> x `mod` p > 0 ) (factorsToTry x)
    factorsToTry x = takeWhile (\p -> p*p <= x) primes

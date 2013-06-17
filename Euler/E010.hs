-- E10.hs

{-# OPTIONS_GHC -Wall #-}

module Euler.E010 ( e010 ) where

-- sum of primes under 2,000,000
e010 :: Integer
e010 = fromIntegral $ sum $ primesUnder 2000000
primesUnder :: Int -> [Int]
primesUnder n = primeSieve [] [2..n-1]
  where
    primeSieve :: [Int] -> [Int] -> [Int]
    primeSieve sieved [] = sieved
    primeSieve sieved (x:xs) = primeSieve (x:sieved) unsieved
      where
        unsieved
          | x*x < n   = filter (\a -> mod a x /= 0) xs
          | otherwise = xs

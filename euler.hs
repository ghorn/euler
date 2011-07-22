-- euler.hs

module Main where

e1 :: Integer
e1 = sum $ filter (\x -> divisible x 5 || divisible x 3) [1..999]
  where
    divisible :: (Integral a) => a -> a -> Bool
    divisible a b = if mod a b == 0 then True else False


e2 :: Integer
e2 = sum $ filter even (fibsUnder 4000000)
  where
    fibsUnder :: Integer -> [Integer]
    fibsUnder n = f n []
      where
        f :: Integer -> [Integer] -> [Integer]
        f n []  = f n [1]
        f n [x] = f n (2:[x])
        f n (x:xs)
          | x + (head xs) > n    = (x:xs)
          | otherwise = f n ((x + (head xs)):(x:xs))


e3 :: Integer
e3 = head $ factorize 600851475143 []
  where
    factorize :: Integer -> [Integer] -> [Integer]
    factorize 1 knownFactors = knownFactors
    factorize n knownFactors = factorize (n `div` firstDivisor) (firstDivisor:knownFactors)
      where
        firstDivisor = head $ filter (\x -> mod n x == 0) [2..n]


e4 :: Integer
e4 = maximum [a*b | a <- [100..999], b <- [100..a], show (a*b) == reverse (show (a*b))]


-- a faster analytic solution would be to multiply all the greatest power of the prime factorization
e5 :: Integer
e5 = head $ filter alldiv [1..]
  where
    alldiv n = all (\x -> mod n x == 0) [20,19..2]


-- difference between sum of squares and square of sum of [1..100]
e6 :: Integer
e6 = (sqrSum 100) - (sumSqr 100)
  where
    sumSqr n = sum $ map (\x -> x*x) [1..n]
    sqrSum n = (\x -> x*x) $ sum [1..n]

-- the 10,001st prime number
e7 :: Int
e7 = head $ nPrimes 10001
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
        isPrime x = and $ map (\a -> mod x a /= 0) sieved


-- only pythagorean triplet with perimiter == 1000
e9 :: Int
e9 = head [ a*b*c | a <- [1..1000], b <- [1..a], c <- [1000 - a - b], a*a + b*b == c*c]


-- sum of primes under 2,000,000
e10 = sum $ primesUnder 2000000
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


main = do
  print ("e1", e1 == 233168)
  print ("e2", e2 == 4613732)
  print ("e3", e3 == 6857)
  print ("e4", e4 == 906609)
  print ("e5", e5 == 232792560)
  print ("e6", e6 == 25164150)
  print ("e7", e7 == 104743)
  
  print ("e9", e9 == 31875000)  
  print ("e10", e10 == 142913828922)

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

e5 :: Integer
e5 = head $ filter alldiv [1..]
  where
    alldiv n = all (\x -> mod n x == 0) [20,19..2]
-- a faster analytic solution would be to multiply all the greatest power of the prime factorization

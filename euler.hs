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


-- greatest product of 5 consecutive digits in a 1000-digit number
e8 :: Integer
e8 = maximum $ products5 $ digits num
  where
    digits :: Integer -> [Integer]
    digits 0 = []
    digits n = digits (div n 10) ++ [(mod n 10)]

    products5 :: [Integer] -> [Integer]
    products5 xs = if length xs > 4 then (product $ take 5 xs):(products5 $ tail xs) else []

    num = 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450


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


-- greatest sum along binary tree (dynamic programming)
e18 :: Integer
e18 = head $foldr (\x acc -> zipWith (+) x (maxNeighbor' acc)) (last tree) (init tree)
--e18 = head $foldr (\x acc -> zipWith (+) x (maxNeighbor [] acc)) (last tree) (init tree)
  where
    tree = [[75],
            [95,64],
            [17,47,82],
            [18,35,87,10],
            [20,04,82,47,65],
            [19,01,23,75,03,34],
            [88,02,77,73,07,63,67],
            [99,65,04,28,06,16,70,92],
            [41,41,26,56,83,40,80,70,33],
            [41,48,72,33,47,32,37,16,94,29],
            [53,71,44,65,25,43,91,52,97,51,14],
            [70,11,33,28,77,73,17,78,39,68,17,57],
            [91,71,52,38,17,14,91,43,58,50,27,29,48],
            [63,66,04,68,89,53,67,30,73,16,69,87,40,31],
            [04,62,98,27,23,09,70,98,73,93,38,53,60,04,23]]

    maxNeighbor :: [Integer] -> [Integer] -> [Integer]
    maxNeighbor acc [_] = acc
    maxNeighbor acc (x0:x1:xs) = maxNeighbor (acc ++ [max x0 x1]) (x1:xs)

    maxNeighbor' :: [Integer] -> [Integer]
    maxNeighbor' xs = snd $ foldl (\(xPrev,acc) x -> (x, acc ++ [max x xPrev])) (head xs, []) (tail xs)


main = do
  print ("e1", e1 == 233168)
  print ("e2", e2 == 4613732)
  print ("e3", e3 == 6857)
  print ("e4", e4 == 906609)
  print ("e5", e5 == 232792560)
  print ("e6", e6 == 25164150)
  print ("e7", e7 == 104743)
  print ("e8", e8 == 40824)
  print ("e9", e9 == 31875000)  
  print ("e10", e10 == 142913828922)

  print ("e18", e18 == 1074)

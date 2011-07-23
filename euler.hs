-- euler.hs

module Main where

import Data.List --(elemIndex)
import Data.Maybe (isNothing, fromJust)
import qualified Data.Map
import GHC.Exts (sortWith)
import Data.Time.Clock (getCurrentTime,diffUTCTime)
import System.IO (hFlush,stdout)
import System (getArgs)


-- the IO is here, the problems are all below
main :: IO ()
main = do

  triangle <- readFile "triangle.txt"

  args <- getArgs

  let fastSols = [("e01", e1, 233168),
                  ("e02", e2, 4613732),
                  ("e03", e3, 6857),
                  ("e04", e4, 906609),
                  ("e05", e5, 232792560),
                  ("e06", e6, 25164150),
                  ("e08", e8, 40824),
                  ("e09", e9, 31875000),
                  ("e10", e10, 142913828922),
                  ("e13", e13, 5537376230),
                  ("e18", e18, 1074),
                  ("e67", e67 triangle, 7273)]

      slowSols = [("e07", e7, 104743)]

      allSols = sortWith (\(x,_,_) -> x) (fastSols ++ slowSols)

      sols
        | (elem "--fast" args) || (elem "-f" args) = fastSols
        | (elem "--slow" args) || (elem "-s" args) = slowSols
        | otherwise                                = allSols

  let profile :: (Integral a) => (String, a, a) -> IO Bool
      profile (name, fcn, trueAnswer) = do
        putStr $ "evaluating " ++ name ++ "... "
        hFlush stdout
        start    <- getCurrentTime
        putStr $ if (fcn == trueAnswer) then "correct! " else "fail >:( "
        end      <- getCurrentTime
        putStrLn $ "eval time: " ++ show (diffUTCTime end start)
        return (fcn == trueAnswer)

  start   <- getCurrentTime
  results <- mapM profile sols
  end     <- getCurrentTime
  if elem False results
    then putStrLn $ "\n" ++ (show (length (filter (== False) results))) ++ " incorrect answers"
    else putStrLn "\nno errors"
  putStrLn $ "total eval time: " ++ show (diffUTCTime end start)


-- all of the problems follow
e1 :: Integer
e1 = sum $ filter (\x -> divisible x 5 || divisible x 3) [1..999]
  where
    divisible :: (Integral a) => a -> a -> Bool
    divisible a b = if mod a b == 0 then True else False


e2 :: Integer
e2 = sum $ filter even (fibsUnder 4000000 [])
  where
    fibsUnder :: Integer -> [Integer] -> [Integer]
    fibsUnder n []  = fibsUnder n [1]
    fibsUnder n [x] = fibsUnder n (2:[x])
    fibsUnder n (x:xs)
      | x + (head xs) > n    = (x:xs)
      | otherwise = fibsUnder n ((x + (head xs)):(x:xs))


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


-- difference between sum of squares and square of sum of [1..100]
e6 :: Integer
e6 = (sqrSum 100) - (sumSqr 100)
  where
    sumSqr n = sum $ map (\x -> x*x) [1..n]
    sqrSum n = (\x -> x*x) $ sum [1..n]

-- the 10,001st prime number
e7 :: Integer
e7 = fromIntegral $ head $ nPrimes 10001
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
e9 :: Integer
e9 = head [ a*b*c | a <- [1..1000], b <- [1..a], c <- [1000 - a - b], a*a + b*b == c*c]


-- sum of primes under 2,000,000
e10 :: Integer
e10 = fromIntegral $ sum $ primesUnder 2000000
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


-- first 10 digits of sum of 100 50-digit numbers
e13 :: Integer
e13 = read $ take 10 $ show (sum nums)
  where
    nums :: [Integer]
    nums = [37107287533902102798797998220837590246510135740250,
            46376937677490009712648124896970078050417018260538,
            74324986199524741059474233309513058123726617309629,
            91942213363574161572522430563301811072406154908250,
            23067588207539346171171980310421047513778063246676,
            89261670696623633820136378418383684178734361726757,
            28112879812849979408065481931592621691275889832738,
            44274228917432520321923589422876796487670272189318,
            47451445736001306439091167216856844588711603153276,
            70386486105843025439939619828917593665686757934951,
            62176457141856560629502157223196586755079324193331,
            64906352462741904929101432445813822663347944758178,
            92575867718337217661963751590579239728245598838407,
            58203565325359399008402633568948830189458628227828,
            80181199384826282014278194139940567587151170094390,
            35398664372827112653829987240784473053190104293586,
            86515506006295864861532075273371959191420517255829,
            71693888707715466499115593487603532921714970056938,
            54370070576826684624621495650076471787294438377604,
            53282654108756828443191190634694037855217779295145,
            36123272525000296071075082563815656710885258350721,
            45876576172410976447339110607218265236877223636045,
            17423706905851860660448207621209813287860733969412,
            81142660418086830619328460811191061556940512689692,
            51934325451728388641918047049293215058642563049483,
            62467221648435076201727918039944693004732956340691,
            15732444386908125794514089057706229429197107928209,
            55037687525678773091862540744969844508330393682126,
            18336384825330154686196124348767681297534375946515,
            80386287592878490201521685554828717201219257766954,
            78182833757993103614740356856449095527097864797581,
            16726320100436897842553539920931837441497806860984,
            48403098129077791799088218795327364475675590848030,
            87086987551392711854517078544161852424320693150332,
            59959406895756536782107074926966537676326235447210,
            69793950679652694742597709739166693763042633987085,
            41052684708299085211399427365734116182760315001271,
            65378607361501080857009149939512557028198746004375,
            35829035317434717326932123578154982629742552737307,
            94953759765105305946966067683156574377167401875275,
            88902802571733229619176668713819931811048770190271,
            25267680276078003013678680992525463401061632866526,
            36270218540497705585629946580636237993140746255962,
            24074486908231174977792365466257246923322810917141,
            91430288197103288597806669760892938638285025333403,
            34413065578016127815921815005561868836468420090470,
            23053081172816430487623791969842487255036638784583,
            11487696932154902810424020138335124462181441773470,
            63783299490636259666498587618221225225512486764533,
            67720186971698544312419572409913959008952310058822,
            95548255300263520781532296796249481641953868218774,
            76085327132285723110424803456124867697064507995236,
            37774242535411291684276865538926205024910326572967,
            23701913275725675285653248258265463092207058596522,
            29798860272258331913126375147341994889534765745501,
            18495701454879288984856827726077713721403798879715,
            38298203783031473527721580348144513491373226651381,
            34829543829199918180278916522431027392251122869539,
            40957953066405232632538044100059654939159879593635,
            29746152185502371307642255121183693803580388584903,
            41698116222072977186158236678424689157993532961922,
            62467957194401269043877107275048102390895523597457,
            23189706772547915061505504953922979530901129967519,
            86188088225875314529584099251203829009407770775672,
            11306739708304724483816533873502340845647058077308,
            82959174767140363198008187129011875491310547126581,
            97623331044818386269515456334926366572897563400500,
            42846280183517070527831839425882145521227251250327,
            55121603546981200581762165212827652751691296897789,
            32238195734329339946437501907836945765883352399886,
            75506164965184775180738168837861091527357929701337,
            62177842752192623401942399639168044983993173312731,
            32924185707147349566916674687634660915035914677504,
            99518671430235219628894890102423325116913619626622,
            73267460800591547471830798392868535206946944540724,
            76841822524674417161514036427982273348055556214818,
            97142617910342598647204516893989422179826088076852,
            87783646182799346313767754307809363333018982642090,
            10848802521674670883215120185883543223812876952786,
            71329612474782464538636993009049310363619763878039,
            62184073572399794223406235393808339651327408011116,
            66627891981488087797941876876144230030984490851411,
            60661826293682836764744779239180335110989069790714,
            85786944089552990653640447425576083659976645795096,
            66024396409905389607120198219976047599490197230297,
            64913982680032973156037120041377903785566085089252,
            16730939319872750275468906903707539413042652315011,
            94809377245048795150954100921645863754710598436791,
            78639167021187492431995700641917969777599028300699,
            15368713711936614952811305876380278410754449733078,
            40789923115535562561142322423255033685442488917353,
            44889911501440648020369068063960672322193204149535,
            41503128880339536053299340368006977710650566631954,
            81234880673210146739058568557934581403627822703280,
            82616570773948327592232845941706525094512325230608,
            22918802058777319719839450180888072429661980811197,
            77158542502016545090413245809786882778948721859617,
            72107838435069186155435662884062257473692284509516,
            20849603980134001723930671666823555245252804609722,
            53503534226472524250874054075591789781264330331690]


-- greatest sum along small binary tree
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

    maxNeighbor' :: [Integer] -> [Integer]
    maxNeighbor' xs = snd $ foldl (\(xPrev,acc) x -> (x, acc ++ [max x xPrev])) (head xs, []) (tail xs)

--    maxNeighbor :: [Integer] -> [Integer] -> [Integer]
--    maxNeighbor acc [_] = acc
--    maxNeighbor acc (x0:x1:xs) = maxNeighbor (acc ++ [max x0 x1]) (x1:xs)


-- (non-working brute force solution) sum of all numbers whose sum of factorial of digits == number
e34 :: Integer
e34 = sum $ filter good [1..10000]
  where
    splitIntegral :: (Integral a, Read a) => a -> [a]
    splitIntegral a = map (\x -> read x) $ map (\x -> [x]) (show a)

    fact :: (Integral a) => a -> a
    fact 0 = 1
    fact n = n *(fact (n-1))

    sumFact n = sum $ map fact (splitIntegral n)
    good :: Integer -> Bool
    good n = (sumFact n) == n


-- greatest sum along large binary tree
e67 :: String -> Integer
e67 fileText = head $foldr (\x acc -> zipWith (+) x (maxNeighbor' acc)) (last tree) (init tree)
  where
    tree = fileToTree fileText

    maxNeighbor' :: [Integer] -> [Integer]
    maxNeighbor' xs = snd $ foldl (\(xPrev,acc) x -> (x, acc ++ [max x xPrev])) (head xs, []) (tail xs)

    fileToTree :: String -> [[Integer]]
    fileToTree x = map (map read) $ map words $ map init (lines x)


-- break list of ordered triples (login attempts)
e79 :: Integer
e79 = head $ filter validPassword [1..]
  where
    keylog :: (Integral a) => [a]
    keylog = [319,680,180,690,129,620,762,689,762,318,
              368,710,720,710,629,168,160,689,716,731,
              736,729,316,729,729,710,769,290,719,680,
              318,389,162,289,162,718,729,319,790,680,
              890,362,319,760,316,729,380,319,728,716]

    sLog :: (Integral a, Read a) => [[a]]
    sLog = map splitIntegral keylog

    splitIntegral :: (Integral a, Read a) => a -> [a]
    splitIntegral a = map (\x -> read x) $ map (\x -> [x]) (show a)

    match' :: (Eq a) => [a] -> [a] -> Bool
    match' [] _ = True  -- everthing has been succesfully matched
    match' _ [] = False -- still trying to match more but reached the end of the list
    match' (x:xs) test
      | isNothing (elemIndex x test) = False
      | otherwise                    = match' xs nextTest
      where
        nextTest = drop (idx + 1) test
        idx = fromJust (elemIndex x test)

    validPassword :: Integer -> Bool
    validPassword p = and $ map (\x -> match' x (splitIntegral p)) sLog

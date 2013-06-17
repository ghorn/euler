{-# OPTIONS_GHC -Wall #-}

module Euler.E096 ( main, isSolved, sumIs45Filter, blah ) where

import Control.Monad ( foldM )
import Data.IntSet ( IntSet, (\\) )
import qualified Data.IntSet as IS
import Data.List ( transpose )
import Data.Maybe
import Data.Vector ( Vector, (!), (//) )
import qualified Data.Vector as V

import Euler.E96Data ( e96Data )

parse :: [String] -> [[[Int]]]
parse xs = case splitAt 10 xs of
  ([],_) -> []
  (x,others) -> parseOnePuzzle x:parse others
    where
      parseOneLine :: String -> [Int]
      parseOneLine = map (\d -> read [d])

      parseOnePuzzle :: [String] -> [[Int]]
      parseOnePuzzle (_:blahs) = map parseOneLine blahs
      parseOnePuzzle _ = error "parseOnePuzzle fail"

allCombos :: [[Int]]
allCombos = rows ++ cols ++ blocks
  where
    rows :: [[Int]]
    rows = map row [0..8]
      where
        row r = map (9*r +) [0..8]
    
    cols :: [[Int]]
    cols = transpose rows
    
    blocks :: [[Int]]
    blocks = [getBlock rows kr kc | kr <- [0..2], kc <- [0..2]]
      where
        getBlock :: [[Int]] -> Int -> Int -> [Int]
        getBlock xs kr kc = concatMap filterRow filteredCols
          where
            filterRow :: [Int] -> [Int]
            filterRow rs = map (rs !!) rowInds
            
            filteredCols :: [[Int]]
            filteredCols = map (xs !!) colInds
            
            rowInds, colInds :: [Int]
            rowInds = map (kr*3 +) [0..2]
            colInds = map (kc*3 +) [0..2]

rawPuzzles :: [[[Int]]]
rawPuzzles = parse e96Data

data Entry = Known Int | Unknown IntSet deriving Eq
data Sudoku = Sudoku !(Vector Entry) deriving Eq

instance Show Sudoku where
  show (Sudoku v) = init $ unlines $ map (concatMap show) entries
    where
      entries :: [[Entry]]
      entries = f $ V.toList v

      f x = case splitAt 9 x of
        ([],_) -> []
        (x', others) -> x':(f others)

instance Show Entry where
  show (Known k) = show k
  show (Unknown ks) = show (IS.toList ks)

isKnown :: Entry -> Bool
isKnown (Known _) = True
isKnown _ = False

isUnknown :: Entry -> Bool
isUnknown = not . isKnown

toEntry :: Int -> Entry
toEntry 0 = Unknown (IS.fromList [1..9])
toEntry k = Known k

toSudoku :: [[Int]] -> Sudoku
toSudoku = Sudoku . V.fromList . (map toEntry) . concat

puzzles :: [Sudoku]
puzzles = map toSudoku rawPuzzles

totalSum :: Int
totalSum = sum [1..9]

-- take a list of entries, collect the known numbers, and filter these out of the unknown numbers
-- e.g. simpleFilter [Know 3, Unknown [1,2,3], Unknown [3,4]] == [Known 3, Unknown [1,2], Known 4]
simpleFilter :: [Entry] -> Maybe [Entry]
simpleFilter entries0
  | IS.size knownSet /= length knownList = Nothing -- two knowns are the same - invalid puzzle
  | entries0 /= entries = simpleFilter entries
  | otherwise = Just entries
  where
    entries = map myFilter entries0
    
    knownSet :: IntSet
    knownSet = IS.fromList knownList
    
    knownList = foldr g [] entries0
      where
        g (Known k) acc = k : acc
        g _ acc = acc

    myFilter :: Entry -> Entry
    myFilter (Unknown oldSet)
      | newSetSize == 1 = Known (IS.findMin newSet)
      | otherwise = Unknown newSet
      where
        newSetSize = IS.size newSet
        newSet = oldSet \\ knownSet
    myFilter known@(Known _) = known

blah :: [Entry]
blah = [Known 1, Known 2, Known 3, Known 4, Unknown (IS.fromList [5,7,8]), Known 6, Unknown (IS.fromList [7,8]), Unknown (IS.fromList [5,7]), Known 9]

--sumIs45Filter :: [Entry] -> Maybe [Entry]
sumIs45Filter entries0 = unknowns
  where
    knownSum = sum $ map (\(Known k) -> k) $ filter isKnown entries0
    remainder = totalSum - knownSum

    unknowns = map (\(Unknown k) -> IS.toList k) $ filter isUnknown entries0
    
--    unknowns

filterGroup :: Sudoku -> [Int] -> Maybe Sudoku
filterGroup (Sudoku v0) idxs = case simpleFilter (map (v0 !) idxs) of
  Nothing -> Nothing
  Just newGroup -> Just $ Sudoku $ v0 // (zip idxs newGroup)

--combinations :: [[Int]] -> [[Int]]
--combinations (x:xs) = concatMap (\x' -> x':combos) x
--  where
--    combos = combinations xs

filterAll :: Sudoku -> Maybe Sudoku
filterAll sudoku0
  | isNothing sudoku' = Nothing
  | sudoku0 /= sudoku = filterAll sudoku
  | otherwise = Just sudoku
  where
    sudoku' = foldM filterGroup sudoku0 allCombos
    sudoku = fromJust sudoku'

--guess :: (Int,Int) -> Sudoku -> Maybe Sudoku
--guess (idx, pickMe) (Sudoku v0) = filterAll (Sudoku v)
--  where
--    v = v0 // [(idx, newSet)]
--    newSet = case v0 ! idx of
--      (Known _) -> error "guess was told to replace an already solved number"
--      (Unknown is) -> if IS.member pickMe is
--                      then Known pickMe
--                      else error $ "guess was told to guess a number " ++
--                           show pickMe ++ " but that isn't one of the candidates " ++ show (IS.toList is)

isSolved :: Sudoku -> Bool
isSolved (Sudoku v) = V.all isKnown v

filteredPuzzles :: [Maybe Sudoku]
filteredPuzzles = map filterAll puzzles

main :: IO ()
main = do
  print (filteredPuzzles !! 1)
--  putStrLn ""
--  print (filteredPuzzles !! 1 >>= guess (1,4) >>= guess (2,5))

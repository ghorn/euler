-- E14.hs

{-# OPTIONS_GHC -Wall #-}

module Euler.E14(e14, e14') where

import Data.IntMap(IntMap, member, insert, singleton, (!), assocs)
import Data.Foldable(foldr')

buildLenMap :: Int -> IntMap Int
buildLenMap n = foldr' insertLen (singleton 1 1) [2..n]

nextNumber :: Integral a => a -> a
nextNumber n
  | even n = n `div` 2
  | odd  n = 3*n + 1
  | otherwise = error "neither even nor odd"
                
insertLen :: Int -> IntMap Int -> IntMap Int
insertLen n lmap0
  | member n lmap0 = lmap0
  | otherwise      = insert n (1 + (lmap1 ! nextN)) lmap1
  where
    nextN = nextNumber n
    -- this is the bottleneck:
    lmap1 = insertLen nextN lmap0

-- slow because of garbage collection
e14 :: Integer
e14 = fromIntegral $ fst $ foldr' max' (0,0) (assocs lmap)
  where
    lmap = buildLenMap 1000000
    max' (k, v) (kOld, vOld)
      | v > vOld  = (k, v)
      | otherwise = (kOld, vOld)

-- much faster but sad because it's brute force
e14' :: Integer
e14' = fromIntegral $ fst $ foldr' f (0,0) [1..1000000]
  where
    f k (kOld, vOld)
      | v > vOld  = (k, v)
      | otherwise = (kOld, vOld)
      where
        v = seqLen k

seqLen :: Int -> Int
seqLen = f 1
  where
    f acc 1 = acc
    f acc n = f (acc + 1) (nextNumber n)

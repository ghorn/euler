-- E67.hs

{-# OPTIONS_GHC -Wall #-}

module Euler.E067 ( e067 ) where

-- greatest sum along large binary tree
e067 :: String -> Integer
e067 fileText = head $foldr (\x acc -> zipWith (+) x (maxNeighbor' acc)) (last tree) (init tree)
  where
    tree = fileToTree fileText

    maxNeighbor' :: [Integer] -> [Integer]
    maxNeighbor' xs = snd $ foldl (\(xPrev,acc) x -> (x, acc ++ [max x xPrev])) (head xs, []) (tail xs)

    fileToTree :: String -> [[Integer]]
    fileToTree x = map (map read) $ map words $ map init (lines x)

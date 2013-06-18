{-# OPTIONS_GHC -Wall #-}

module Euler.E015 ( e015 ) where

import Data.Packed.Matrix
import Data.Packed.ST ( STMatrix, runSTMatrix, readMatrix, writeMatrix, newMatrix )
import Control.Monad.ST ( ST )

chooseRoutes :: Int -> STMatrix s Int -> Int -> ST s ()
chooseRoutes _ _ (-1) = return ()
chooseRoutes n m k = do
  let allIndexes = reverse $ [(k,c) | c <- [k..n-1]] ++ [(r,k) | r <- [k+1..n-1]]
      writeSum (r,c) = do
        let sumThese = choices n (r,c)
        blah <- fmap sum $ mapM (\(x,y) -> readMatrix m x y) sumThese
        writeMatrix m r c blah
  mapM_ writeSum allIndexes
  chooseRoutes n m (k-1)
  

choices :: Int -> (Int,Int) -> [(Int,Int)]
choices n (k,j)
  | k+1 == n && j+1 == n = []
  | k+1 == n = [(k,j+1)]
  | j+1 == n = [(k+1,j)]
  | otherwise = [(k,j+1),(k+1,j)]

routeMatrix :: Int -> Matrix Int
routeMatrix n' = runSTMatrix $ do
  let n = n' + 1
  m <- newMatrix 0 n n
  writeMatrix m (n-1) (n-1) 1
  chooseRoutes n m (n-2)
  return m

e015 :: Integer
e015 = fromIntegral $ (routeMatrix 20) @@> (0,0)

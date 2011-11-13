-- euler.hs

{-# OPTIONS_GHC -Wall #-}

module Main where

import GHC.Exts(sortWith)
import Data.Time.Clock(getCurrentTime,diffUTCTime)
import System.IO(hFlush,stdout)
import System.Environment(getArgs)

import Euler.E1(e1)
import Euler.E2(e2)
import Euler.E3(e3)
import Euler.E4(e4)
import Euler.E5(e5)
import Euler.E6(e6)
import Euler.E7(e7, e7')
import Euler.E8(e8)
import Euler.E9(e9)
import Euler.E10(e10)
import Euler.E13(e13)
import Euler.E14(e14, e14')
import Euler.E18(e18)
import Euler.E67(e67)

-- run all the problems
main :: IO ()
main = do

  triangle <- readFile "triangle.txt"

  args <- getArgs

  let fastSols = [ ("e01", e1, 233168)
                 , ("e02", e2, 4613732)
                 , ("e03", e3, 6857)
                 , ("e04", e4, 906609)
                 , ("e05", e5, 232792560)
                 , ("e06", e6, 25164150)
                 , ("e07'", e7', 104743)
                 , ("e08", e8, 40824)
                 , ("e09", e9, 31875000)
                 , ("e10", e10, 142913828922)
                 , ("e13", e13, 5537376230)
                 , ("e14'", e14', 837799)
                 , ("e18", e18, 1074)
                 , ("e67", e67 triangle, 7273)
                 ]

      slowSols = [ ("e07", e7, 104743)
                 , ("e14", e14, 837799)
                 ]

      allSols = sortWith (\(x,_,_) -> x) (fastSols ++ slowSols)

      sols
        | (elem "--fast" args) || (elem "-f" args) = fastSols
        | (elem "--slow" args) || (elem "-s" args) = slowSols
        | otherwise                                = allSols

  let profile :: (Integral a) => (String, a, a) -> IO Bool
      profile (name, fcn, trueAnswer) = do
        putStr $ "evaluating " ++ name ++ "......\t"
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


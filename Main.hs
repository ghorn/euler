-- euler.hs

{-# OPTIONS_GHC -Wall #-}

module Main where

import GHC.Exts(sortWith)
import Data.Time.Clock(getCurrentTime,diffUTCTime)
import System.IO(hFlush,stdout)
import System.Environment(getArgs)

import Euler.E001 ( e001 )
import Euler.E002 ( e002 )
import Euler.E003 ( e003 )
import Euler.E004 ( e004 )
import Euler.E005 ( e005 )
import Euler.E006 ( e006 )
import Euler.E007 ( e007 )
import Euler.E008 ( e008 )
import Euler.E009 ( e009 )
import Euler.E010 ( e010 )
import Euler.E011 ( e011 )
import Euler.E013 ( e013 )
import Euler.E014 ( e014, e014' )
import Euler.E015 ( e015 )
import Euler.E016 ( e016 )
import Euler.E018 ( e018 )
import Euler.E020 ( e020 )
import Euler.E067 ( e067 )

-- run all the problems
main :: IO ()
main = do

  triangle <- readFile "triangle.txt"

  args <- getArgs

  let fastSols = [ ("e001",  e001, 233168)
                 , ("e002",  e002, 4613732)
                 , ("e003",  e003, 6857)
                 , ("e004",  e004, 906609)
                 , ("e005",  e005, 232792560)
                 , ("e006",  e006, 25164150)
                 , ("e007",  e007, 104743)
                 , ("e008",  e008, 40824)
                 , ("e009",  e009, 31875000)
                 , ("e010",  e010, 142913828922)
                 , ("e011",  e011, 70600674)
                 , ("e013",  e013, 5537376230)
                 , ("e014'", e014', 837799)
                 , ("e015", e015, 137846528820)
                 , ("e016", e016, 1366)
                 , ("e018",  e018, 1074)
                 , ("e020", e020, 648)
                 , ("e067",  e067 triangle, 7273)
                 ]

      slowSols = [ ("e014", e014, 837799)
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

